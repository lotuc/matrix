(ns tiltontec.cell.diagnostic
  #?(:cljs
     (:require-macros [tiltontec.cell.diagnostic :refer [mxtrc mxtrc-cell]]
                      [tiltontec.util.ref :refer [any-ref?]]))
  (:require
   #?(:clj [tiltontec.util.ref :refer [any-ref?]])
   [clojure.set :as set]
   [tiltontec.cell.base
    :refer [c-async? c-md-name c-prop-name c-ref? c-value c-value-state
            md-ref?]]
   [tiltontec.util.core :refer [mx-type set-ify]]))

;;; a collection of tags to be traced (checkout `mxtrc` & `mxtrc-cell`)
(def ^:dynamic *mx-trace* nil)

;;; a function which receives model and returns debug info of it. checkout
;;; `minfo` for its default.
(def ^:dynamic *mx-minfo* nil)

(defn c-debug?
  ([c] (c-debug? c :annon))
  ([c tag]
   (when-let [dbg (:debug @c)]
     (or (true? dbg)
         (= dbg tag)
         (and (coll? dbg) (some #{tag} dbg))))))

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
    :else [(c-prop-name c)
           (c-md-name c)
           (:mx-sid @c)
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

(defn build-trace-map [& bits]
  (assert (even? (count bits)))
  (->> (partition 2 bits)
       (map (fn [[k v]]
              (assert (keyword? k) (str "build-trace-map> key is not keyword |" k "|"))
              [k v]))
       (into {})))

(defn print-trace [trc-fn tag trace-map]
  (binding [*print-level* (or *print-level* 3)]
    (println (str trc-fn ">" tag ">") (pr-str trace-map))))

(defmacro mxtrc
  "Prints bits if tag is in `*mx-trace*`."
  [tag & bits]
  (assert (or (keyword? tag) (and (vector? tag) (every? keyword? tag)))
          (str "mxtrc> first argument must be keyword or keywords to trace, not |" tag "|"))
  `(when (match-loose ~tag *mx-trace*)
     (print-trace :mxtrc ~tag (build-trace-map ~@bits))))

(defmacro mxtrc-cell
  "Print bits if tag is in `*mx-trace*` and `c` is true or a cell which
  `:debug` is on."
  [c tag & bits]
  (assert (or (keyword? tag) (and (vector? tag) (every? keyword? tag)))
          (str "mxtrc> first argument must be keyword or keywords to trace, not |" tag "|"))
  `(when ~c
     (binding [*print-level* (or *print-level* 3)]
       (assert (or (= ~c true) (c-ref? ~c))
               (str "mxtrc-cell> passed non c-ref? " ~tag "| " (if (any-ref? ~c) @~c ~c)))
       (when (and (or (= ~c true) (:debug @~c))
                  (match-loose ~tag *mx-trace*))
         (print-trace :mxtrc-cell ~tag (build-trace-map ~@bits))))))

(comment
  (mxtrc "not ok")
  (mxtrc :boom :msg "hi mom")
  (binding [*mx-trace* [:boom :bom]]
    (mxtrc :bom "not ok"))
  (binding [*mx-trace* [:boom :bom]]
    (mxtrc :bom :msg "hi mom"))
  (binding [*mx-trace* [:boom :bom]]
    ;; vector tags
    (mxtrc [:bom :abc] :msg "hi mom"))
  (binding [*mx-trace* [:boom :bom]]
    (mxtrc-cell true :boom :msg "hi mon")))
