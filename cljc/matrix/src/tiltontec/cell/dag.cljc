(ns tiltontec.cell.dag
  (:require
   [clojure.string :as str]
   [tiltontec.cell.base :refer [c-callers c-md-name c-prop-name c-useds]]))

;;; --- dag dump utility ----------------------------------------

(def ^:dynamic *dag-depth*
  "How far we are in edges from the starting node."
  0)

(def ^:dynamic *dag-prn-len*
  "How many edges to follow from users/callers."
  5)

(def +dag-visited+
  "Which DAG nodes have been dumped so far."
  (atom #{}))

(defn dag-prn [& os]
  (let [path (apply str (repeat *dag-depth* ".|"))]
    (apply println path os)))

(def ^:dynamic *dag-node-only-printer*
  (fn [tag c] (dag-prn tag :PM! (c-prop-name c) :of (c-md-name c))))

(declare dag-dump-1)

(defn dag-dump-callers [c]
  (let [ccs (take (or *dag-prn-len* 999)
                  (c-callers c))]
    (when (seq ccs)
      (binding [*dag-depth* (inc *dag-depth*)]
        (doseq [cc ccs]
          ;; (dag-prn :used-by (c-prop-name cc) :of (c-md-name cc))
          (dag-dump-1 :used-by cc))))))
(def ^:dynamic *dag-callers-printer* dag-dump-callers)

(defn dag-dump-useds [c]
  (let [ccs (take (or *dag-prn-len* 999)
                  (c-useds c))]
    (when (seq ccs)
      (binding [*dag-depth* (inc *dag-depth*)]
        (doseq [cc ccs]
          ;; (dag-prn :using (c-prop-name cc) :of (c-md-name cc))
          (dag-dump-1 :using cc))))))
(def ^:dynamic *dag-useds-printer* dag-dump-useds)

(defn dag-dump-1 [tag c]
  (cond
    (contains? @+dag-visited+ c)
    (dag-prn (str/upper-case (str tag ": " (c-prop-name c) "/" (c-md-name c))))
    :else (do
            (swap! +dag-visited+ conj c)
            (when-let [p *dag-node-only-printer*]
              (p tag c))
            (when-let [p *dag-callers-printer*]
              (p c))
            (when-let [p *dag-useds-printer*]
              (p c)))))

(defn dag-dump [tag c]
  (reset! +dag-visited+ #{})
  (binding [*dag-depth* 0]
    (dag-dump-1 tag c)))
