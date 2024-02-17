(ns tiltontec.cell.evaluate
  (:require
   #?(:clj  [tiltontec.cell.integrity :refer [c-current? c-pulse-update with-integrity]]
      :cljs [tiltontec.cell.integrity
             :refer-macros [with-integrity]
             :refer [c-current? c-pulse-update]])
   #?(:clj  [tiltontec.util.ref
             :refer [dosync! meta-map-set-prop! meta-map-swap-prop! ref-set!
                     ref-swap! rmap-set-prop!]]
      :cljs [tiltontec.util.ref
             :refer-macros [dosync! meta-map-set-prop! meta-map-swap-prop!
                            ref-set! ref-swap! rmap-set-prop!]])
   #?(:clj [tiltontec.util.core :refer [mx-type prog1 throw-ex]]
      :cljs [tiltontec.util.core
             :refer [mx-type]
             :refer-macros [prog1 throw-ex]])
   [clojure.string :as str]
   #?(:clj  [tiltontec.cell.base
             :refer [*c-prop-depth* *call-stack* *causation* *custom-propagator*
                     *defer-changes* *depender* *one-pulse?* *pulse* c-callers c-code$
                     c-ephemeral? c-formula? c-input? c-lazy-but-not-until-asked?
                     c-md-name c-me c-model c-optimize c-optimized-away? c-prop
                     c-prop-name c-pulse c-pulse-last-changed c-pulse-unwatched?
                     c-pulse-watched c-ref? c-rule c-state c-synaptic? c-useds c-valid?
                     c-value c-value-state c-warn dependency-drop dependency-record
                     md-dead? unbound unlink-from-callers unlink-from-used
                     without-c-dependency]
             :as cty]
      :cljs [tiltontec.cell.base
             :refer [*c-prop-depth* *call-stack* *causation* *custom-propagator*
                     *defer-changes* *depender* *one-pulse?* *pulse* c-callers c-code$
                     c-ephemeral? c-formula? c-input? c-lazy-but-not-until-asked?
                     c-md-name c-me c-model c-optimize c-optimized-away? c-prop
                     c-prop-name c-pulse c-pulse-last-changed c-pulse-unwatched?
                     c-pulse-watched c-ref? c-rule c-state c-synaptic? c-useds c-valid?
                     c-value c-value-state dependency-drop dependency-record
                     md-dead? unbound unlink-from-callers unlink-from-used]
             :refer-macros [without-c-dependency c-warn]
             :as cty])
   #?(:clj  [tiltontec.cell.diagnostic :refer [cinfo minfo mxtrc]]
      :cljs [tiltontec.cell.diagnostic
             :refer [cinfo minfo]
             :refer-macros [mxtrc]])
   [tiltontec.cell.poly :refer [c-awaken md-quiesce md-quiesce-self
                                unchanged-test watch]]))

;;; API Summary
;;; - `cget`
;;; - `c-quiesce`
;;; - `ensure-value-is-current`: check dependencies
;;; - `c-value-assume`: record cell's new value & propagate to dependents

;;; More details

;;; - `calculate-and-set`
;;;   - calculate a value using `calculate-and-link`, which records dependencies
;;;   - record calculated value using `c-value-assume` and propagate to dependent

;;; - `propagate`: notify callers of change & call watchs
;;;   - the actual propagate and (potential ephemeral-reset) are deferred (check
;;;     `propagate-to-callers` for explanation)
;;;   - means propagate operations are actually proceed when current pulse's
;;;     change all done; and ephemerals are reset after that

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn- c-watch
  "Do watch on cell & record the watched pulse (Checkout
  `c-pulse-unwatched?`)."
  ([c why]
   (c-watch c unbound why))
  ([c prior-value _why]
   (assert (c-ref? c))
   (assert (integer? @*pulse*))
   (rmap-set-prop! c :pulse-watched @*pulse*)
   (watch (c-prop c) (c-model c) (c-value c) prior-value c)
   (when-let [cw (:watch @c)]
     (cw (c-prop c) (c-model c) (c-value c) prior-value c))))

(defn- cget-value
  "Checkout `tiltontec.cell.base/c-value` &
  `tiltontec.cell.base/c-optimized-away?`.

  returns [value-state/:non-ref value]."
  [c]
  (if (c-ref? c)
    (let [v @c]
      (if (and (map? v) (contains? v ::cty/state))
        [(::cty/state v) (:value v)]
        ;; optimized away cell
        [:optimized-away v]))
    [:non-ref c]))

(defn- c-value-changed? [c new-value old-value]
  (not ((or (:unchanged-if @c)
            (unchanged-test (c-model c) (c-prop c)))
        new-value old-value)))

(defn- ephemeral-reset [rc]
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
        (when-let [p (c-prop rc)]
          (ref-swap! me assoc p nil)))
      (ref-swap! rc assoc :value nil))))

(declare calculate-and-set calculate-and-link c-value-assume)

(defn ensure-value-is-current
  "The key to data integrity: recursively check the known dependency
  graph to decide if we are current, and if not kick off recalculation
  and propagation."
  [c]
  (cond
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
    (some-> (c-model c) md-dead?)
    (throw-ex "evic> model of cell is dead" {:cell c})

    ;; --- no more early exits  -------------------
    (or (not (c-valid? c))
        (loop [[used & urest] (seq (c-useds c))]
          (when used
            (mxtrc [:ensure-value-is-current :check-used]
                   :cinfo (cinfo c) :used-cinfo (cinfo used))
            (ensure-value-is-current used)
            ;; now see if it actually changed; maybe it just got made current because no
            ;; dependency was out of date. If so, that alone does not mean we need to re-run.
            (or (when-let [last-changed (c-pulse-last-changed used)]
                  (> last-changed (c-pulse c)))
                (recur urest)))))
    (do (when-not (c-current? c)
          ;; Q: how can it be current after above checks indicating not current?
          ;; A: if dependent changed during above loop over used and its watch read/updated me
          (calculate-and-set c))
        (second (cget-value c)))

    ;; we were behind the pulse but not affected by the changes that moved the pulse
    ;; record that we are current to avoid future checking:
    :else
    (do (c-pulse-update c :valid-uninfluenced)
        (c-value c))))

(defn cget
  "The API for determing the value associated with a Cell.
  Ensures value is current, records any dependent, and
  notices if a standalone cell has never been watched."
  [c]
  (let [[value-state v] (cget-value c)]
    ;; opti-way goes in stages.
    (if (#{:non-ref :optimized-away} value-state)
      v
      (prog1
       (do
         (mxtrc [:cget] :cinfo (cinfo c))
         (with-integrity []
           (assert (c-ref? c) "c lost c-refness")
           (let [prior-value (c-value c)]
             (prog1
              (ensure-value-is-current c)

              ;; this is new here, intended to awaken standalone cells JIT
              ;; /do/ might be better inside evic, or test here
              ;; to see if c-model is nil? (trying latter...)
              (when (and (nil? (c-model c))
                         (= (c-state c) :nascent)
                         (c-pulse-unwatched? c))
                (rmap-set-prop! c ::cty/state :awake)
                (c-watch c prior-value :cget)
                (ephemeral-reset c))))))
       (when *depender*
         (dependency-record c))))))

(defn- calculate-and-set
  "Calculate, link, record, and propagate."
  [c]
  (let [[raw-value propagation-code] (calculate-and-link c)]
    ;; TODO: handling (c-async? c).
    (when-not (c-optimized-away? c)
      (assert (map? (deref c)) "calc-n-set")
      ;; this check for optimized-away? arose because a rule using without-c-dependency
      ;; can be re-entered unnoticed since that "clears" *call-stack*. If re-entered, a subsequent
      ;; re-exit will be of an optimized away cell, which will have been value-assumed
      ;; as part of the opti-away processing.
      (mxtrc [:calculate-and-set :not-optimized] :cinfo (cinfo c))
      (c-value-assume c raw-value propagation-code))))

(defn- prop-info-&-callstack
  "Stringify cell's belonging model & prop-name & the current callstack."
  [c]
  (let [prop (c-prop-name c)
        code (c-code$ c)
        md-name (c-md-name c)]
    (str "prop '" prop "' of model '" md-name "'."
         "\n...> formula for " prop ":\n" code
         "\n...> full cell:\n" @c
         "\n\n...> callstack, latest first:\n"
         (str/join "\n"
                   (for [cd *call-stack*
                         :let [md-name' (c-md-name cd)
                               prop' (c-prop-name cd)
                               code' (c-code$ cd)]]
                     (str "....> md-name:" md-name' " prop: " prop'
                          "\n....>    code:" code'))))))

(defn- calculate-and-link
  "The name is accurate: we do no more than invoke the rule of a formula
  and return its value*, but along the way the links between
  dependencies and dependents get determined anew.

  * Well, we also look to see if a synaptic cell has attached a propagaion code
  to a vector used to wrap the raw value, which we then unpack."
  [c]
  (when (some #{c} *call-stack*)
    (c-warn "MXAPI_COMPUTE_CYCLE_DETECTED> cyclic dependency detected while computing " (prop-info-&-callstack c))
    (throw-ex "MXAPI_COMPUTE_CYCLE_DETECTED> cyclic dependency detected while computing cell" {:cell c}))
  (mxtrc [:calculate-and-link :entry] :cinfo (cinfo c))

  (binding [*call-stack* (cons c *call-stack*)
            *depender* c
            *defer-changes* true]
    (unlink-from-used c :pre-rule-clear)
    (let [rule (c-rule c)]
      (assert rule (str "No rule in " (c-prop-name c) " type " (mx-type c)))
      (try
        (let [raw-value (rule c)]
          ;; synaptic cell's raw value is wrapped within vector along with
          ;; metadata indicates if the value should be propgated.
          (if (and (c-synaptic? c)
                   (vector? raw-value)
                   (contains? (meta raw-value) :propagate))
            [(first raw-value) (:propagate (meta raw-value))]
            [raw-value nil]))
        (catch #?(:clj Exception :cljs js/Error) e
          (mxtrc [:calculate-and-link :emsg]
                 :cinfo (cinfo c)
                 :emsg #?(:clj (.getMessage e) :cljs (.message ^js/Error e)))
          (throw e))))))

;;; --- awakening ------------------------------------

(defmethod c-awaken :default [c]
  (if (coll? c)
    (doseq [ce c] (c-awaken ce))
    (throw (ex-info "cannot do c-awaken on cell" {:cell c}))))

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
       (calculate-and-set c)))))

;; ------------------------------------------------------------

(declare optimize-away?! propagate)

(defn c-value-assume
  "The Cell assumes a new value at awakening, on c-reset!, or after
  formula recalculation.

  We record the new value, set the Cell state to :awake, make its
  pulse current, check to see if a formula cell can be optimized away,
  and then propagate to any dependent formula cells."
  [c new-value propagation-code]

  (assert (c-ref? c))
  (mxtrc [:c-value-assume :entry]
         :cinfo (cinfo c) :new-value new-value :propagation-code propagation-code)

  (without-c-dependency
   (let [prior-value (c-value c)
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
     (c-pulse-update c :propv-assume)
     (when (and (not (c-optimized-away? c))
                (not force-no-propagate?)
                value-changed?)
       (rmap-set-prop! c :pulse-last-changed @*pulse*))

     (mxtrc [:c-value-assume :new-value-installed]
            :cinfo (cinfo c) :new-value new-value)

     ;; we optimize here because even if unchanged we may not have c-useds,
     ;; now that, with the :freeze option, we are doing "late" optimize-away
     (when (and (c-formula? c) (c-optimize c))
       (optimize-away?! c prior-value)
       (mxtrc [:c-value-assume :post-optimized-away]
              :cinfo (cinfo c) :new-value new-value :optimized-away? (c-optimized-away? c)))

     (when (or (not (#{:valid :uncurrent} prior-state))
               force-propagate?
               (when-not force-no-propagate? value-changed?))
       ;; --- something happened / data flow propagation -----------
       (when-not (c-optimized-away? c)
         (assert (map? @c))
         (mxtrc [:c-value-assume :calls-propagate]
                :callers-count (count callers) :prior-value prior-value)
         (propagate c prior-value callers)))))

  new-value)

(defn- md-cell-flush
  "Flushes it from its model if it has one & record the flush."
  [c]
  (when-let [me (c-model c)]
    (let [p (c-prop c)
          r [p :val (c-value c) :pulse (c-pulse-watched c)]]
      (meta-map-swap-prop! me :cz assoc p nil)
      ;; recording the flush
      (meta-map-swap-prop! me :cells-flushed conj r)
      (mxtrc [:optimize :md-cell-flush] :cinfo (cinfo c) :minfo (minfo me)))))

;; --- optimize away -------------------------------------------------------
;; optimizing away cells who turn out not to depend on anyone saves a lot of
;; work at runtime.

(defn- optimize-away?!
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
      (mxtrc [:optimize :optimize-away!] :cinfo (cinfo c))

      (when (:on-quiesce @c)
        (c-warn "optimize-away?!> on-quiesce detected on cell, "
                "but it will be ignored since the cell is being optimized away "
                (prop-info-&-callstack c)))

      (when (= :freeze (c-optimize c))
        (unlink-from-used c :freeze))

      (rmap-set-prop! c ::cty/state :optimized-away)
      (c-watch c prior-value :opti-away)

      (md-cell-flush c)

      ;; let callers know they need not check us for currency again
      (doseq [caller (seq (c-callers c))]
        (mxtrc [:optimize :runs-caller]
               :cinfo (cinfo c) :caller-cinfo (cinfo caller))
        (ensure-value-is-current caller)
        (when-not (c-optimized-away? caller)
          (dependency-drop c caller)))

      (ref-set! c (c-value c)))))

;;--------------- change propagation  ----------------------------

(declare propagate-to-callers)

(defn- propagate
  "A cell:
  - notifies its callers of its change;
  - calls any watch; and
  - if ephemeral, silently reverts to nil."
  ;; /do/ support other values besides nil as the "resting" value

  [c prior-value callers]
  (mxtrc [:propagate :entry] :cinfo (cinfo c))
  (if *one-pulse?*
    (when *custom-propagator*
      (*custom-propagator* c prior-value))

    ;; ----------------------------------
    (binding [*depender* nil
              *call-stack* nil
              *c-prop-depth* (inc *c-prop-depth*)
              *defer-changes* true]

      (propagate-to-callers c callers)

      (when-not (c-optimized-away? c)
        ;; they get watched at the time
        (when (or (c-pulse-unwatched? c)
                  ;; messy: these can get setfed/propagated twice in one pulse+
                  (c-lazy-but-not-until-asked? c))
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

(defn- warn-on-propgate-to-dead-model [c]
  (c-warn
   (let [prop (c-prop-name c)
         code (c-code$ c)
         md-name (c-md-name c)]
     (str "propgate-to-callers> dead by time :tell-deps dispatched; bailing"
          "prop '" prop "' of model '" md-name "'."
          "\n...> formula for " prop ":\n" code
          "\n...> full cell:\n" @c))))

(defn- propagate-to-callers [c callers]
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
      (with-integrity [:tell-dependents (cinfo c)]
        (mxtrc [:with-integrity :tell-dependents] :cinfo (cinfo c))
        (if (md-dead? (c-model c))
          (warn-on-propgate-to-dead-model c)
          (binding [*causation* causation]
            (doseq [caller (seq callers)
                    :let [skip-propagation?
                          (or ;; lotsa reasons NOT to proceed
                           (= (c-state caller) :quiesced)
                           ;; happens if I changed when caller used me in current pulse+
                           (c-current? caller)
                           (c-lazy-but-not-until-asked? caller))]
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
              (mxtrc [:propagate :noti-caller]
                     :caller-cinfo (cinfo caller) :callee-cinfo (cinfo c))
              (calculate-and-set caller))))))))

;; --- c-quiesce -----------

(defn c-quiesce [c]
  (assert (c-ref? c))
  (when-let [onq (:on-quiesce @c)] (onq c))
  (unlink-from-callers c)
  (unlink-from-used c :quiesce)
  (ref-set! c :dead-c))

;; --- md-quiesce --

(defmethod md-quiesce-self :default [me]
  (mxtrc [:quiesce :qself-fallthru] :minfo (minfo me))
  (when-let [onq (:on-quiesce (meta me))] (onq me))
  ;; cell's quiesce won't be execued if it's been optimized away
  (doseq [c (vals (:cz (meta me))) :when c] (c-quiesce c))
  (ref-set! me nil)
  (meta-map-set-prop! me ::cty/state :dead))

(defmethod md-quiesce :default [me]
  (mxtrc [:quiesce :def-fall-thru!] :minfo (minfo me))
  (md-quiesce-self me))
