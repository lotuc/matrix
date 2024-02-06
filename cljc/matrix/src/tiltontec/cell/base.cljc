(ns tiltontec.cell.base
  #?(:cljs (:require-macros
            [tiltontec.cell.base :refer [un-stopped without-c-dependency]]
            [tiltontec.util.ref :refer [any-ref? def-rmap-props dosync! make-ref
                                        ref-swap!]]))
  (:require
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])
   #?(:clj [tiltontec.util.ref
            :refer [any-ref? def-rmap-props dosync! make-ref ref-swap!]])
   #?(:clj [tiltontec.util.trace :refer [pr-warn]]
      :cljs [tiltontec.util.trace :refer-macros [pr-warn]])
   [tiltontec.util.core :refer [mx-type?]]))

;; --- the Cells beef -----------------------

(defn pulse-initial [] (make-ref 0))

(def ^:dynamic *pulse* (pulse-initial))
(def +client-q-handler+ (atom nil))

;;; seems to be a debug related falg, when true, propagate values using
;;; *custom-propagator* (when not set, no propagate is done).
(def ^:dynamic *one-pulse?* false)
(def ^:dynamic *custom-propagator* nil)

(def ^:dynamic *dp-log* false)

(def ^:dynamic *causation* '())
(def ^:dynamic *call-stack* nil)
(def ^:dynamic *depender*
  "*depender* let's us differentiate between the call stack and
and dependency. The problem with overloading *call-stack* with both roles
is that we miss cyclic reentrance when we use without-c-dependency in a
rule to get once behavior or just when fm-traversing to find someone"
  nil)

(def ^:dynamic *defer-changes* false)

(defn pulse-now [] @*pulse*)
(defn cells-init [] (dosync! (ref-swap! *pulse* (constantly 0))))

(defonce unbound (gensym "unbound-cell-value"))

;;; todo: seems that value won't ever been set to uncurrent
(defonce uncurrent (gensym "uncurrent-formulaic-value"))

(def ^:dynamic *quiesce* false)

;;; --- unfinished business post state change ------------------------

(def +ufb-opcodes+ [:tell-dependents
                    :awaken
                    :client
                    :ephemeral-reset
                    :change])

(defn unfin-biz-build [] (into {} (for [i +ufb-opcodes+] [i (make-ref [])])))

(def ^:dynamic *unfinished-business* (unfin-biz-build))

(def ^:dynamic *within-integrity* false)

;; --- debug stuff -----------------------------

(def ^:dynamic *c-prop-depth* 0)

;; emergency brake
(def ^:dynamic +stop+ (atom false))

(defn cells-reset
  ([] (cells-reset {}))
  ([options]
   (reset! *pulse* 0)
   (reset! +client-q-handler+ (:client-queue-handler options))))

(defmacro without-c-dependency [& body]
  `(binding [*depender* nil]
     ~@body))

(defmacro un-stopped [& body]
  `(when-not @+stop+
     ~@body))

(defmacro c-warn [& args]
  `(when-not @+stop+
     (pr-warn ~@args)))

;; ------------------------------------------------------

(derive ::model ::object)
(derive ::cell ::object)
(derive ::c-formula ::cell)

(defn c-formula? [c]
  (mx-type? c ::c-formula))

(defn c-ref? [x]
  (and (any-ref? x)
       (mx-type? x ::cell)))

(def-rmap-props c-
  me prop state input? rule pulse pulse-last-changed pulse-watched
  useds users callers optimize ephemeral? code
  lazy synapses synaptic? async?)

(defn c-model [rc] (c-me rc))

(defn c-prop-name [rc] (c-prop rc))

(defn c-code$ [c]
  (with-out-str (binding [*print-level* 20]
                  (pprint (:code @c)))))

(defn c-value [c]
  (assert (any-ref? c))
  (if (and (c-ref? c) (map? @c))
    (:value @c)
    @c))

(defn c-optimized-away? [c]
  (assert (c-ref? c) "c-awy?-got-not-c")
  (or (not (map? @c))
      (not (contains? @c ::state))
      (= :optimized-away (::state @c))))

(defn c-optimized-away-value
  "Return a vector wrapped value of a cell which has been optimized
  away, or nil if it has not."
  [c]
  (assert (c-ref? c) "c-awy?-got-not-c")
  (let [v @c]
    (cond
      ;; non-cell, which means a optimized away value.
      (or (not (map? v)) (not (contains? v ::state))) [v]
      ;; state indicated optimized away, extract the value
      (= :optimized-away (::state v)) [(:value v)])))

(defn c-md-name [c]
  (if-let [md (c-model c)]
    (or (:name @md) "anon")
    "no-md"))

(defn c-value-state [rc]
  (condp = (c-value rc)
    unbound :unbound
    uncurrent :uncurrent
    :valid))

(defn c-unbound? [rc] (= :unbound (c-value-state rc)))

(defn c-valid? [rc] (= :valid (c-value-state rc)))

(defn c-pulse-unwatched? [c]
  (if-let [pulse-watched (c-pulse-watched c)]
    (> @*pulse* pulse-watched)
    true))

;; --- dependency maintenance --------------------------------

(defn dependency-record [used]
  (when-not (c-optimized-away? used)
    (ref-swap! *depender* update :useds (fnil conj #{}) used)
    (ref-swap! used update :callers (fnil conj #{}) *depender*)))

(defn dependency-drop [used caller]
  (ref-swap! caller update :useds (fnil disj #{}) used)
  (ref-swap! used update :callers (fnil disj #{}) caller))

(defn unlink-from-callers [used]
  (doseq [caller (c-callers used)]
    (dependency-drop used caller)))

(defn unlink-from-used
  "Tell dependencies they need not notify us when they change, then
  clear our record of them."
  [caller _why]
  (doseq [used (c-useds caller)]
    (dependency-drop used caller)))

;; debug aids --------------

(defn c-props [c k]
  (assert (c-ref? c))
  (set (map c-prop (k @c))))

;; --- defmodel rizing ---------------------

(defn md-ref? [x] (any-ref? x))

;; hhack (mx-type? x ::model)

;; --- mdead? ---

(defn md-state [me] (::state (meta me)))

(defn mdead? [me] (= :dead (md-state me)))

;;---

(defn md-prop-owning? [_class-name _prop-name] false) ;; hhack
