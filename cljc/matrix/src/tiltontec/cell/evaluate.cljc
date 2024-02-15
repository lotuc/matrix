(ns tiltontec.cell.evaluate
  #?(:cljs (:require-macros
            [tiltontec.util.ref
             :refer [any-ref? dosync! meta-map-set-prop! meta-map-swap-prop!
                     ref-set! ref-swap! rmap-set-prop!]]))
  (:require
   #?(:cljs [tiltontec.cell.integrity
             :refer-macros [with-integrity]
             :refer [c-current? c-pulse-update]]
      :clj  [tiltontec.cell.integrity :refer [c-current? c-pulse-update with-integrity]])
   #?(:cljs [tiltontec.util.trace :refer-macros [trx]]
      :clj  [tiltontec.util.trace :refer [trx]])
   #?(:clj [tiltontec.util.ref
            :refer [any-ref? dosync! meta-map-set-prop! meta-map-swap-prop!
                    ref-set! ref-swap! rmap-set-prop!]])
   #?(:clj [tiltontec.util.core :refer [mx-type prog1 set-ify throw-ex]]
      :cljs [tiltontec.util.core
             :refer [mx-type set-ify throw-ex]
             :refer-macros [prog1]])
   [clojure.set :refer [difference]]
   [clojure.string :as str]
   [tiltontec.cell.base
    :refer [*c-prop-depth* *call-stack* *causation* *custom-propagator*
            *defer-changes* *depender* *one-pulse?* *pulse* *quiesce*
            c-callers c-code$ c-ephemeral? c-formula? c-input? c-lazy
            c-md-name c-me c-model c-optimize c-optimized-away-value
            c-optimized-away? c-prop c-prop-name c-pulse c-pulse-last-changed
            c-pulse-unwatched? c-pulse-watched c-ref? c-rule c-state
            c-synaptic? c-unbound? c-useds c-valid? c-value c-value-state
            c-warn dependency-drop dependency-record md-prop-owning? mdead?
            unlink-from-callers unlink-from-used without-c-dependency unbound]
    :as cty]
   [tiltontec.cell.diagnostic :refer [c-debug? cinfo minfo mxtrc mxtrc-cell]]
   [tiltontec.cell.poly :refer [c-awaken md-quiesce md-quiesce-self
                                unchanged-test watch]]))

(defn c-watch
  ([c why]
   (c-watch c unbound why))
  ([c prior-value _why]
   (assert (c-ref? c))
   (assert (integer? @*pulse*))
   (rmap-set-prop! c :pulse-watched @*pulse*)
   (watch (c-prop c) (c-model c) (c-value c) prior-value c)
   (when-let [cw (:watch @c)]
     (cw (c-prop c) (c-model c) (c-value c) prior-value c))))

(defn ephemeral-reset [rc]
  ;; allow call on any cell, catch here
  (when (c-ephemeral? rc)
    ;;
    ;; as of Cells3 we defer resetting ephemerals because everything
    ;; else gets deferred and we cannot /really/ reset it until
    ;; within finish_business we are sure all callers have been recalculated
    ;; and all watchs completed (which happens with recalc).
    ;;
    (with-integrity [:ephemeral-reset rc]
      (when-let [me (c-model rc)]
        ;; presumption next is that model cells live in
        ;; their own internal prop of model FNYI
        (ref-swap! me assoc (c-prop rc) nil))
      (ref-swap! rc assoc :value nil))))

(declare calculate-and-set cget-value-as-is)

(defn ensure-value-is-current
  "The key to data integrity: recursively check the known dependency
  graph to decide if we are current, and if not kick off recalculation
  and propagation."

  [c _debug-id ensurer]

  (cond
    ; --------------------------------------------------
    *quiesce*
    ; we got kicked off during md-quiesce processing
    ; just return what we have if valid, else nil
    (cond
      (c-unbound? c)
      (do
        (trx :unbound!!! c-prop)
        (throw-ex "evic> unbound prop %s of model %s" {:cell c}))

      (c-valid? c) ;; probably accomplishes nothing
      (c-value c))

    ;; --- easy way out: our pulse is current ---------------
    (c-current? c)
    (c-value c)

    ;; --- also easy with an optimize edge case lost to history -------
    (and (c-input? c)
         (c-valid? c)
         ;; a cFn (ruled-then-input) cell will not be valid at first
         (not (and (c-formula? c)
                   (= (c-optimize c) :when-value-t)
                   (nil? (c-value c)))))
    (c-value c)

    ;; --- above we had valid values so did not care. now... -------
    (when-let [md (c-model c)]
      (mdead? md))
    (throw-ex "evic> model of cell is dead" {:cell c})

    ;; --- no more early exits  -------------------
    (or (not (c-valid? c))
        (loop [[used & urest] (seq (c-useds c))]
          (when used
            (ensure-value-is-current used :nested c)
            ;; now see if it actually changed; maybe it just got made current because no
            ;; dependency was out of date. If so, that alone does not mean we need to re-run.
            (or (when-let [last-changed (c-pulse-last-changed used)]
                  (> last-changed (c-pulse c)))
                (recur urest)))))
    (let [calc-val (when-not (c-current? c)
                     ;; Q: how can it be current after above checks indicating not current?
                     ;; A: if dependent changed during above loop over used and its watch read/updated me
                     (mxtrc-cell c :evic-sees-uncurrent)
                     (calculate-and-set c :evic ensurer))]
      (mxtrc-cell c :evic-returns :calc-val calc-val)
      (cget-value-as-is c))

    ;; we were behind the pulse but not affected by the changes that moved the pulse
    ;; record that we are current to avoid future checking:
    :else
    (do
      (mxtrc-cell c :just-pulse-valid-uninfluenced)
      (c-pulse-update c :valid-uninfluenced)
      (c-value c))))

(defn cget-value-as-is [c]
  (if (c-ref? c)
    (if (and (map? @c) (contains? @c ::cty/state))
      (:value @c)
      @c)
    c))

(defn cget
  "The API for determing the value associated with a Cell.
  Ensures value is current, records any dependent, and
  notices if a standalone cell has never been watched."
  [c]
  (if (not (c-ref? c))
    c
    ;; opti-way goes in stages.
    (if-some [[v] (c-optimized-away-value c)]
      v
      (prog1
       (with-integrity []
         (assert (c-ref? c) "c lost c-refness")
         (let [prior-value (c-value c)]
           (mxtrc-cell c :cget-core :mx-type (mx-type (c-model c)))
           (prog1
            (let [ci (cinfo c)
                  dbg? (c-debug? c :cget)
                  ev (ensure-value-is-current c :c-read nil)]
              (if (c-ref? c)
                (mxtrc-cell c :cget-post-evic-val :ensured-value ev)
                (mxtrc-cell dbg? :cget-evic-flushed-returns :ensured-value ev :ci-was ci))
              ev)

            ;; this is new here, intended to awaken standalone cells JIT
            ;; /do/ might be better inside evic, or test here
            ;; to see if c-model is nil? (trying latter...)
            (when (and (nil? (c-model c))
                       (= (c-state c) :nascent)
                       (c-pulse-unwatched? c))
              (rmap-set-prop! c ::cty/state :awake)
              (c-watch c prior-value :cget)
              (ephemeral-reset c)))))
       (when *depender*
         (dependency-record c))))))

(declare calculate-and-link
         c-value-assume)

(defn calculate-and-set
  "Calculate, link, record, and propagate."
  [c dbgid _dbgdata]
  (let [[raw-value propagation-code] (calculate-and-link c)]
    (mxtrc-cell c :post-cnlink-sees!!!!
                :dbgid dbgid :opti (c-optimized-away? c) :prop (c-prop c)
                :raw-value raw-value :propagation-code propagation-code)
    ;; TODO: handling (c-async? c).
    (when-not (c-optimized-away? c)
      (assert (map? (deref c)) "calc-n-set")
      ;; this check for optimized-away? arose because a rule using without-c-dependency
      ;; can be re-entered unnoticed since that "clears" *call-stack*. If re-entered, a subsequent
      ;; re-exit will be of an optimized away cell, which will have been value-assumed
      ;; as part of the opti-away processing.
      (mxtrc-cell c :not-optimized!!!!!!!!!!!)
      (c-value-assume c raw-value propagation-code))))

(defn calculate-and-link
  "The name is accurate: we do no more than invoke the rule of a formula
  and return its value*, but along the way the links between
  dependencies and dependents get determined anew.

  * Well, we also look to see if a synaptic cell has attached a
  propagaion code to a vector used to wrap the raw value, which we
  then unpack."
  [c]
  (when (some #{c} *call-stack*)
    (let [prop (c-prop-name c)]
      (c-warn "MXAPI_COMPUTE_CYCLE_DETECTED> cyclic dependency detected while computing prop '"
              prop "' of model '" (c-md-name c) "'.\n"
              "...> formula for " prop ":\n"
              (c-code$ c)
              "\n...> full cell: \n"
              @c
              "\n\n...> callstack, latest first: \n"
              (str/join "\n" (mapv (fn [cd]
                                     (str "....> md-name:" (c-md-name cd) " prop: " (c-prop-name cd)
                                          "\n....>    code:" (c-code$ cd)))
                                   *call-stack*)))
      (throw-ex "MXAPI_COMPUTE_CYCLE_DETECTED> cyclic dependency detected while computing cell" {:cell c})))

  (binding [*call-stack* (cons c *call-stack*)
            *depender* c
            *defer-changes* true]
    (unlink-from-used c :pre-rule-clear)
    (assert (c-rule c) (#?(:clj format :cljs str) "No rule in %s type %s" (:prop c) (type @c)))
    (try
      (let [raw-value ((c-rule c) c)
            ;; synaptic cell's raw value is wrapped within vector along with
            ;; some metadata
            prop-code? (and (c-synaptic? c)
                            (vector? raw-value)
                            (contains? (meta raw-value) :propagate))]
        (mxtrc-cell c :cnlink-raw-val :raw-value raw-value :prop-code? prop-code?)
        (if prop-code?
          [(first raw-value) (:propagate (meta raw-value))]
          [raw-value nil]))
      (catch #?(:clj Exception :cljs js/Error) e
        (mxtrc-cell c :cnlink-emsg :emsg (.getMessage #?(:clj Exception :cljs js/Error) e))
        (throw e)))))

;;; --- awakening ------------------------------------

(defmethod c-awaken :default [c]
  (trx :c-awaken-def!!!)
  (if (coll? c)
    (doseq [ce c]
      (c-awaken ce))
    (trx :c-awaken-fall-thru (if (any-ref? c)
                               [:ref-of (mx-type c) (meta c)]
                               [:unref c (mx-type c) (meta c)]))))

(defmethod c-awaken ::cty/cell [c]
  (assert (c-input? c))
  ;; nothing to calculate, but every cellular prop should be output on birth
  (dosync!
   (when (c-pulse-unwatched? c)
     ;; safeguard against double-call
     (when-let [me (c-me c)]
       (rmap-set-prop! me (c-prop c) (c-value c)))
     (c-watch c :cell-awaken)
     (ephemeral-reset c))))

(defmethod c-awaken ::cty/c-formula [c]
  (dosync!
   ;; hhack -- bundle this up into reusable with evic
   (binding [*depender* nil]
     (when-not (c-current? c)
       (calculate-and-set c :fn-c-awaken nil)))))

;; ------------------------------------------------------------

(declare optimize-away?!
         propagate
         c-value-changed?)

(defn md-prop-value-store [me prop value]
  (assert me)
  (assert (any-ref? me))
  (rmap-set-prop! me prop value))

(defn c-value-assume
  "The Cell assumes a new value at awakening, on c-reset!, or after
  formula recalculation.

  We record the new value, set the Cell state to :awake, make its
  pulse current, check to see if a formula cell can be optimized away,
  and then propagate to any dependent formula cells."
  [c new-value propagation-code]

  (assert (c-ref? c))
  (mxtrc-cell c :cva-entry :new-value new-value :propagation-code propagation-code)

  (without-c-dependency
   (let [dbg? (c-debug? c)
         prior-value (c-value c)
         prior-state (c-value-state c)
         ;; copy callers before possible optimize-away
         callers (c-callers c)
         value-changed? (c-value-changed? c new-value prior-value)
         force-propagate? (true? propagation-code)
         force-no-propagate? (or (false? propagation-code)
                                 ;; a legacy value, I guess
                                 (= propagation-code :no-propagate))]

     ;;
     ;; --- model maintenance ---
     (when-some [m (c-model c)]
       ;; redundant with next check, but logic is impeccable; synapses just
       ;; manage cell state, no model property
       (assert (not (c-synaptic? c)))
       (rmap-set-prop! m (c-prop c) new-value))

     ;; --- cell maintenance ---
     ;; even if no news at least honor the reset!
     ;;
     (rmap-set-prop! c :value new-value)
     (rmap-set-prop! c ::cty/state :awake)
     (mxtrc-cell c :cva-new-value-installed :new-value new-value)
     (c-pulse-update c :propv-assume)
     (when (and (not (c-optimized-away? c))
                (not force-no-propagate?)
                value-changed?)
       (rmap-set-prop! c :pulse-last-changed @*pulse*))

     ;; we optimize here because even if unchanged we may not have c-useds,
     ;; now that, with the :freeze option, we are doing "late" optimize-away
     (when (and (c-formula? c) (c-optimize c))
       (optimize-away?! c prior-value)
       (mxtrc-cell c :cva-post-optimize-away))

     (when (or (not (#{:valid :uncurrent} prior-state))
               force-propagate?
               (when-not force-no-propagate? value-changed?))
       ;; --- something happened / data flow propagation -----------
       (when-not (c-optimized-away? c)
         (assert (map? @c))
         (mxtrc-cell dbg? :cva-calls-propagate :callers-count (count callers) :prior-value prior-value)
         (propagate c prior-value callers)))))

  new-value)

;; --- unlinking ----------------------------------------------

(defn- md-cell-flush
  "Flushes it from its model if it has one & record the flush."
  [c]
  (when-let [me (c-model c)]
    (let [p (c-prop c)
          r [p :val (c-value c) :pulse (c-pulse-watched c)]]
      (meta-map-swap-prop! me :cz assoc p nil)
      ;; recording the flush
      (meta-map-swap-prop! me :cells-flushed conj r)
      (mxtrc-cell c :opti :md-cell-flush (cinfo c) :mi (minfo me)))))

;; --- optimize away -------------------------------------------------------
;; optimizing away cells who turn out not to depend on anyone saves a lot of
;; work at runtime.

(defn optimize-away?!
  "Optimizes away cells who turn out not to depend on anyone,
  saving a lot of work at runtime. A caller/user will not bother
  establishing a link, and when we get to models `cget` will find a
  non-cell in a prop and Just Use It."
  [c prior-value]
  (when-let [optimize (and
                       (c-formula? c)
                       ;; no prop to cache invariant result, so they have to
                       ;; stay around)
                       (not (c-synaptic? c))
                       ;; yes, dependent cells can be input
                       (not (c-input? c))
                       ;; returns optimize switch/mode
                       (c-optimize c))]
    ;; now that cell optimizable & optimize is enabled, check the current cell
    ;; state is optimizable
    (when (and (or (= :freeze optimize)
                   (empty? (c-useds c))
                   (and (= :when-value-t optimize)
                        (some? (c-value c))))
               ;; c-streams (FNYI) may come this way repeatedly even if
               ;; optimized away
               (not (c-optimized-away? c))
               ;; when would this not be the case? and who cares?
               (c-valid? c))
      (when (:on-quiesce @c)
        (let [prop (c-prop-name c)]
          (c-warn "optimize-away?!> on-quiesce detected on cell, but it will be ignored since the cell is being optimized away '"
                  prop "' of model '" (c-md-name c) "'.\n"
                  "...> formula for " prop ":\n"
                  (c-code$ c)
                  "\n...> full cell: \n"
                  @c
                  "\n\n...> callstack, latest first: \n"
                  (str/join "\n" (mapv (fn [cd]
                                         (str "....> md-name:" (c-md-name cd) " prop: " (c-prop-name cd)
                                              "\n....>    code:" (c-code$ cd)))
                                       *call-stack*)))))

      (when (= :freeze (c-optimize c))
        (unlink-from-used c :freeze))

      (rmap-set-prop! c ::cty/state :optimized-away)
      (c-watch c prior-value :opti-away)

      (md-cell-flush c)

      ;; let callers know they need not check us for currency again
      (doseq [caller (seq (c-callers c))]
        (mxtrc-cell c :optimized-away-runs-caller :caller caller)
        (ensure-value-is-current caller :opti-used c)
        (when-not (c-optimized-away? caller)
          (dependency-drop c caller)))

      (ref-set! c (c-value c)))))

;; --- c-quiesce -----------

(defn c-quiesce [c]
  (assert (c-ref? c))
  (when-let [onq (:on-quiesce @c)] (onq c))
  (unlink-from-callers c)
  (unlink-from-used c :quiesce)
  (ref-set! c :dead-c))

;; --- md-quiesce --

(defmethod md-quiesce-self :default [me]
  (mxtrc :quiesce :qself-fallthru (minfo me))
  (when-let [onq (:on-quiesce (meta me))] (onq me))
  ;; cell's quiesce won't be execued if it's been optimized away
  (doseq [c (vals (:cz (meta me))) :when c] (c-quiesce c))
  (ref-set! me nil)
  (meta-map-set-prop! me ::cty/state :dead))

(defmethod md-quiesce :default [me]
  (mxtrc :quiesce :def-fall-thru! (minfo me))
  (md-quiesce-self me))

;----------------- change detection ---------------------------------

(defn c-value-changed? [c new-value old-value]
  (not ((or (:unchanged-if @c)
            (unchanged-test (c-model c) (c-prop c)))
        new-value old-value)))

;;--------------- change propagation  ----------------------------

(declare propagate-to-callers)

(defn propagate
  "A cell:
  - notifies its callers of its change;
  - calls any watch; and
  - if ephemeral, silently reverts to nil."
  ;; /do/ support other values besides nil as the "resting" value

  [c prior-value callers]
  (mxtrc :propagate :entry (cinfo c))
  (if *one-pulse?*
    (when *custom-propagator*
      (*custom-propagator* c prior-value))

    ;; ----------------------------------
    (binding [*depender* nil
              *call-stack* nil
              *c-prop-depth* (inc *c-prop-depth*)
              *defer-changes* true]
      ;; --- manifest new value as needed ---
      ;;
      ;; 20061030 Trying not.to.be first because doomed instances may be interested in callers
      ;; who will decide to propagate. If a family instance kids prop is changing, a doomed kid
      ;; will be out of the kids but not yet quiesced. If the propagation to this rule asks the kid
      ;; to look at its siblings (say a view instance being deleted from a stack who looks to the psib
      ;; pb to decide its own pt), the doomed kid will still have a parent but not be in its kids prop
      ;; when it goes looking for a sibling relative to its position.
      (when (and prior-value
                 (c-model c)
                 ;; TODO: this is always false, meaning that doomed children is
                 ;; not quiesce here.
                 (md-prop-owning? (type (c-model c)) (c-prop c)))
        (when-let [ownees (difference (set-ify prior-value) (set-ify (c-value c)))]
          (doseq [ownee ownees]
            (md-quiesce ownee))))

      (propagate-to-callers c callers)

      (when-not (c-optimized-away? c)
        ;; they get watched at the time
        (when (or (c-pulse-unwatched? c)
                  ;; messy: these can get setfed/propagated twice in one pulse+
                  (some #{(c-lazy c)} [:once-asked :always true]))
          (c-watch c prior-value :propagate)))

      ;;
      ;; with propagation done, ephemerals can be reset. we also do this in c-awaken, so
      ;; let the fn decide if C really is ephemeral. Note that it might be possible to leave
      ;; this out and use the pulse to identify obsolete ephemerals and clear them
      ;; when read. That would avoid ever making again bug I had in which I had the reset
      ;; inside prop-value-watch,
      ;; thinking that that always followed propagation to callers. It would also make
      ;; debugging easier in that I could find the last ephemeral value in the inspector.
      ;; would this be bad for persistent CLOS, in which a DB would think there was still a link
      ;; between two records until the value actually got cleared?
      ;;
      (ephemeral-reset c))))

(defn propagate-to-callers [c callers]
  ;;
  ;;  We must defer propagation to callers because of an edge case in which:
  ;;    - X tells A to recalculate
  ;;    - A asks B for its current value
  ;;    - B must recalculate because it too uses X
  ;;    - if B propagates to its callers after recalculating instead of deferring it
  ;;       - B might tell H to reclaculate, where H decides this time to use A
  ;;       - but A is in the midst of recalculating, and cannot complete until B returns.
  ;;         but B is busy eagerly propagating. "This time" is important because it means
  ;;         there is no way one can reliably be sure H will not ask for A
  ;;
  (when-not (empty? callers)
    (let [causation (cons c *causation*)] ;; closed over below
      (with-integrity [:tell-dependents c]
        (if (mdead? (c-model c))
          (trx "WHOAA!!!! dead by time :tell-deps dispatched; bailing" c)
          (binding [*causation* causation]
            (doseq [caller (seq callers)
                    :let [skip-propagation?
                          (or ;; lotsa reasons NOT to proceed
                           (= (c-state caller) :quiesced)
                           ;; happens if I changed when caller used me in current pulse+
                           (c-current? caller)
                           (some #{(c-lazy caller)} [true :always :once-asked])

                           ;; hard to follow, but it is trying to say
                           ;; "go ahead and notify caller one more time even if I have
                           ;; been optimized away cuz they need to know."
                           ;; Note this is why callers must be supplied, having been copied
                           ;; before the optimization step.
                           #_(and (not (some #{c} (c-useds caller)))
                                  (not (c-optimized-away? c))))]
                    :when (not skip-propagation?)

                    ;; it is trying to say
                    ;; "go ahead and notify caller one more time even if I have
                    ;; been optimized away cuz they need to know." Note this is
                    ;; why callers must be supplied, having been copied before
                    ;; the optimization step.

                    ;; TODO: this means other cell's change causing the caller
                    ;; drops dependencies from `c`, build a test case for this
                    ;; one
                    :when (or (some #{c} (c-useds caller))
                              (c-optimized-away? c))]
              (mxtrc :propagate :noti-caller (cinfo caller) :callee (cinfo c))
              (calculate-and-set caller :propagate c))))))))
