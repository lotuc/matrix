(ns tiltontec.cell.diagnostic
  #?(:cljs
     (:require-macros [tiltontec.cell.diagnostic :refer [mxtrc]]
                      [tiltontec.util.ref :refer [any-ref?]]))
  (:require
   #?(:clj [tiltontec.util.ref :refer [any-ref?]])
   [clojure.set :as set]
   [tiltontec.cell.base
    :refer [c-async? c-md-name c-prop-name c-ref? c-value c-value-state
            md-ref? *call-stack*]]
   [tiltontec.util.core :refer [mx-type set-ify]]
   [clojure.string :as str]))

;;; a collection of tags to be traced (checkout `mxtrc`), or set to `:all` to
;;; trace all tags.
(def ^:dynamic *mx-trace* nil)

;;; a function which receives model and returns debug info of it. checkout
;;; `minfo` for its default.
(def ^:dynamic *mx-minfo* nil)

(defn minfo [me]
  (if *mx-minfo*
    (do
      (assert (fn? *mx-minfo*))
      (*mx-minfo* me))
    (cond
      (nil? me) :NIL-MD
      (not (any-ref? me)) :NOT-ANY-REF
      (not (md-ref? me)) :NOT-MD
      :else [(or (:name @me) :anon)
             (meta me)])))

(defn cinfo [c]
  (cond
    (nil? c) :NIL-C
    (not (any-ref? c)) :NOT-ANY-REF-C
    (not (c-ref? c)) :NOT-C-REF
    :else [(str (c-md-name c) "." (or (c-prop-name c) "no-prop") "." (:mx-sid @c))
           [:pulse (:pulse @c)]
           [:val-state (c-value c) (c-value-state c)]
           [:useds (count (:useds @c))
            :callers (count (:callers @c))]
           (mx-type c)
           (c-async? c)]))

(defn match-loose [seek in]
  (when-not (nil? in)
    (or (= in :all)
        (and (coll? in) (some #{:all} in))
        (seq (set/intersection (set-ify seek) (set-ify in))))))

(defn build-trace-values [& bits]
  (assert (even? (count bits)))
  (->> (partition 2 bits)
       (map (fn [[k v]]
              (assert (keyword? k) (str "build-trace-map> key is not keyword |" k "|"))
              [k v]))
       (apply concat)))

(defn print-trace [tag trace-values]
  (binding [*print-level* (or *print-level* 5)]
    (println (str (str/join "" (map (constantly "  ") *call-stack*)) tag ">")
             (pr-str trace-values))))

(defmacro mxtrc
  "Prints bits if tag is in `*mx-trace*`."
  [tag & bits]
  (assert (or (keyword? tag) (and (vector? tag) (every? keyword? tag)))
          (str "mxtrc> first argument must be keyword or keywords to trace, not |" tag "|"))
  `(when (match-loose ~tag *mx-trace*)
     (print-trace ~tag (build-trace-values ~@bits))))

(comment
  (mxtrc "not ok")
  (mxtrc :boom :msg "hi mom")
  (binding [*mx-trace* [:boom :bom]]
    (mxtrc :bom "not ok"))
  (binding [*mx-trace* [:boom :bom]]
    (mxtrc :bom :msg "hi mom"))
  (binding [*mx-trace* [:boom :bom]]
    ;; vector tags
    (mxtrc [:bom :abc] :msg "hi mom")))
