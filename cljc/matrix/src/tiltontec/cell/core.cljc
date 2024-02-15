(ns tiltontec.cell.core
  #?(:cljs (:require-macros
            [tiltontec.util.ref :refer [rmap-set-prop! dosync! make-ref]]))
  (:require
   #?(:clj [tiltontec.cell.integrity :refer [ufb-add with-integrity]]
      :cljs [tiltontec.cell.integrity :refer-macros [with-integrity]])
   #?(:clj [tiltontec.util.ref :refer [dosync! make-ref rmap-set-prop!]])
   [tiltontec.cell.base
    :refer [*c-prop-depth* *call-stack* *causation* *custom-propagator*
            *defer-changes* *depender* *one-pulse?* *pulse*
            *unfinished-business* *within-integrity* c-async? c-input? c-lazy
            c-model c-prop c-prop-name c-value c-warn pulse-initial unbound
            unfin-biz-build without-c-dependency]
    :as cty]
   [tiltontec.cell.evaluate :refer [c-value-assume cget]]
   [tiltontec.util.core :refer [mx-type throw-ex]]
   [tiltontec.util.trace :refer [mx-sid-next]]))

; todo: stand-alone cells with watchs should be watched when they are made

(def +valid-input-options+
  #{:watch :prop :ephemeral? :unchanged-if
    :value :input? :debug :on-quiesce})

(def +valid-formula-options+
  #{:watch :prop :input? :lazy :optimize :ephemeral? :unchanged-if
    :model :synaptic? :synapse-id
    :code :rule :async? :and-then :debug :on-quiesce})

(defn- c-options-canonicalize [options allowed]
  (assert (even? (count options)))
  (->> (partition 2 options)
       (map (fn [[k v]]
              (assert (allowed k) (str "Cell option invalid: " k ". Only allowed are: " allowed))
              [k v]))
       (into {})))

(defn make-cell [& kvs]
  (let [options (c-options-canonicalize kvs +valid-input-options+)]
    (make-ref
     (merge {:mx-sid             (mx-sid-next) ;; debug aid
             :value              unbound
             ::cty/state         :nascent
             :pulse              nil
             :pulse-last-changed nil
             :pulse-watched      nil
             :callers            #{}
             ;; these stay around between evaluations
             :synapses           #{}
             ;; todo: if a rule branches away from a synapse
             ;;       it needs to be GCed so it starts fresh
             :lazy               false ;; not a predicate (can hold, inter alia, :until-asked)
             :ephemeral?         false
             :input?             true}
            options)
     ;; type goes in meta to be consistent with model
     :meta {:mx-type ::cty/cell})))

(defn make-c-formula [& kvs]
  (let [options (c-options-canonicalize kvs +valid-formula-options+)
        rule (:rule options)]
    (assert rule)
    (assert (fn? rule))
    (make-ref
     (merge {:value              unbound
             :mx-sid             (mx-sid-next)
             ::cty/state         :nascent
             :pulse              nil
             :pulse-last-changed nil
             :pulse-watched      nil
             :callers            #{}
             :useds              #{}
             :lazy               false
             :ephemeral?         false
             ;; this can also be :when-not-nil
             :optimize           true
             ;; not redundant: can start with rule, continue as input
             :input?             false}
            options)
     :meta {:mx-type ::cty/c-formula})))

;;___________________ constructors _______________________________
;; I seem to have created a zillion of these, but I normally
;; use just cI, cF, and cFn (which starts out as cF and becomes cI).
;;

(defmacro c-fn-var [[c] & body]
  `(fn [~c]
     (let [~'me (c-model ~c)
           ~'_cell ~c
           ~'_prop-name (c-prop ~c)
           ~'_cache (c-value ~c)]
       ~@body)))

(defmacro c-fn [& body]
  `(c-fn-var (~'prop-c#) ~@body))

(defmacro cF [& body]
  `(make-c-formula
    :code '~body
    :rule (c-fn ~@body)))

(defmacro cF+ [[& options] & body]
  `(make-c-formula
    ~@options
    :code '~body
    :rule (c-fn ~@body)))

(defmacro cFn [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? true
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro cF+n [[& options] & body]
  `(tiltontec.cell.core/make-c-formula
    ~@options
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? true
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cFonce [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? nil
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro cF1 [& body]
  `(cFonce ~@body))

(defmacro cF_ [[& options] & body]
  `(make-c-formula
    ~@options
    :code '~body
    :lazy true
    :rule (c-fn ~@body)))

(defmacro c_F [[& options] & body]
  "Lazy until asked, then eagerly propagating"
  `(make-c-formula
    ~@options
    :code '~body
    :lazy :until-asked
    :rule (c-fn ~@body)))

;; todo add validation somewhere of lazy option

(defmacro c-formula [[& kvs] & body]
  `(make-c-formula
    :code '~body
    :rule (c-fn ~@body)
    ~@kvs))

(defmacro cf-freeze [& [value-form]]
  (let [value-source (or value-form '_cache)]
    `(do
       (rmap-set-prop! ~'_cell :optimize :freeze)
       ~value-source)))

(defmacro with-c-associating [& body]
  `(let [curr# (if (= ~'_cache tiltontec.cell.base/unbound) {} ~'_cache)]
     (if-let [key-values# (do ~@body)]
       (merge curr# (apply hash-map key-values#))
       curr#)))

(defmacro with-c-accumulating [& body]
  `(let [curr# (if (= ~'_cache tiltontec.cell.base/unbound) 0 ~'_cache)]
     (if-let [[new-op# new-value#] (do ~@body)]
       (new-op# curr# new-value#)
       curr#)))

(defmacro with-c-conj [initial-value & body]
  `(let [curr# (if (= ~'_cache tiltontec.cell.base/unbound)
                 ~initial-value
                 ~'_cache)]
     (if-let [new-elt# (do ~@body)]
       (conj curr# new-elt#)
       curr#)))

(defmacro with-c-latest [& body]
  `(let [curr# (when-not (= ~'_cache tiltontec.cell.base/unbound) ~'_cache)]
     (or (do ~@body)
         curr#)))

(defn cI [value & option-kvs]
  (apply make-cell
         :value value
         :input? true
         option-kvs))

;; --- where change and animation begin -------

(defn cset!
  "The moral equivalent of a Common Lisp SETF, and indeed
  in the CL version of Cells SETF itself is the change API dunction."
  [c new-value]
  (assert c)
  (assert (not (c-async? c)) (str "attempt to cset! cfuture " @c))
  (when (not (c-input? c))
    (let [me (c-model c)]
      (c-warn
       "MXAPI_ILLEGAL_MUTATE_NONINPUT_CELL> invalid mswap!/mset!/mset! to the property '" (c-prop-name c) "', which is not mediated by an input cell.\n"
       "..> if such post-make mutation is in fact required, wrap the initial argument to model.core/make in 'cI', 'cFn', or 'cF+n'. eg: (make... :answer (cFn <computation>)).\n"
       "..> look for MXAPI_ILLEGAL_MUTATE_NONINPUT_CELL in the Matrix Errors documentation for  more details.\n"
       "..> FYI: intended new value is [" new-value "].\n"
       "..> FYI: the non-input cell is " @c "\n"
       (when me
         (str "..> FYI: instance is of type " (mx-type me) ".\n"
              "..> FYI: full instance is " @me "\n"
              "..> FYI: instance meta is " (meta me) "\n.")))
      (throw-ex "invalid cset! to non-input cell"
                {:cell c :new-value new-value})))
  (when *defer-changes*
    (let [prop (c-prop-name c)
          me (c-model c)]
      (c-warn
       "MXAPI_UNDEFERRED_CHANGE> undeferred mswap!/mset!/mset! to the property '" prop "' by an watch detected."
       "...> such mutations must be wrapped by WITH-INTEGRITY, must conveniently with macro WITH-CC."
       "...> look for MXAPI_UNDEFERRED_CHANGE in the Errors documentation for  more details.\n"
       "...> FYI: intended new value is [" new-value "]; current value is [" (get @me prop :no-such-prop) "].\n"
       (when me
         (str
          "...> FYI: instance is of type " (mx-type me) ".\n"
          "...> FYI: full instance is " @me "\n"
          "...> FYI: instance meta is " (meta me) "\n.")))
      (throw-ex "change to must be deferred by wrapping it in WITH-INTEGRITY"
                {:cell c :new-value new-value})))

  (if (some #{(c-lazy c)} [:once-asked :always true])
    (c-value-assume c new-value nil)
    (dosync!
     (with-integrity [:change (c-prop c)]
       (c-value-assume c new-value nil)))))

(defn c-reset! [c new-value]
  (cset! c new-value))

(defn c-swap! [c swap-fn & swap-fn-args]
  (cset! c (apply swap-fn (cget c) swap-fn-args)))

(defmacro c-reset-next! [f-c f-new-value]
  "watchs should have side-effects only outside the
cell-mediated model, but it can be useful to have an watch
kick off further change to the model. To achieve this we
allow an watch to explicitly queue a c-reset! for
execution as soon as the current change is manifested."
  `(if *within-integrity*
     (ufb-add :change
              [:c-reset-next!
               (fn [~'opcode ~'defer-info]
                 (call-c-reset-next! ~f-c ~f-new-value))])
     ;; todo new error to test and document
     (throw-ex "c-reset-next! deferred change to %s not under WITH-INTEGRITY supervision."
               {:f-c ~f-c :f-new-value ~f-new-value})))

(defmacro cset-next!
  "Completely untested!!!!!!!!!!!!!!!"
  [f-c f-new-value]
  `(c-reset-next! ~f-c ~f-new-value))

(defn call-c-reset-next! [c new-value]
  (cond
    ;;-----------------------------------
    (some #{(c-lazy c)} [:once-asked :always true])
    (c-value-assume c new-value nil)
    ;;-------------------------------------------
    :else
    (#?(:cljs do :clj dosync)
     (c-value-assume c new-value nil))))

(defn call-with-mx [fn]
  (binding [*pulse* (pulse-initial)
            *within-integrity* false
            *unfinished-business* (unfin-biz-build)
            *causation* '()
            *call-stack* nil
            *depender* nil
            *defer-changes* false
            *c-prop-depth* 0
            *one-pulse?* false
            *custom-propagator* nil]
    (fn)))

(defmacro with-mx [& body]
  `(call-with-mx
    (fn [] ~@body)))
