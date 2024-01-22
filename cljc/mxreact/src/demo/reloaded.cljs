(ns demo.reloaded
  (:require
   [tiltontec.cell.core :as mc]
   [tiltontec.cell.evaluate :as ce]
   [tiltontec.matrix.api :as mx]
   [tiltontec.model.base :as mb]
   [tiltontec.model.core :as md]
   [tiltontec.util.core :as ut]))

(defn- navigate-matrix-model [model f]
  ;; make sure nothing matches, so that we can go through all model
  (let [f' (fn [m] (f m) false)]
    (mx/fm-navig f' model :must? false :inside? true :up? false :me? true)))

(defn watch-model-inputs! [model !state]
  (let [!position->model-prop (atom {})]
    (navigate-matrix-model
     model
     (fn [m]
       (when-some [model-name (md/md-name m)]
         (doseq [[prop-name v] (mb/md-cz m)]
           (when (ut/any-ref? v)
             (when (:input? @v)
               (swap! !position->model-prop assoc [model-name prop-name] [m prop-name])))))))
    (ce/cget (mx/cF+ [:watch (mx/fn-watch (reset! !state new))]
                     (reduce (fn [v [position [m prop-name]]]
                               (assoc-in v position (mx/mget? m prop-name)))
                             {}
                             @!position->model-prop)))))

(defn restore-model-inputs! [model !state]
  (when @!state
    (navigate-matrix-model
     model
     (fn [m]
       (when-some [model-name (md/md-name m)]
         (doseq [[k v] (mb/md-cz m)]
           (when (ut/any-ref? v)
             (when (:input? @v)
               (mc/cset! v (get-in @!state [model-name k]))))))))))

(defn restore-and-watch-model-inputs!
  "A flawed reloaded workflow support.

  saves all named model's **input** props by model name & prop name. Notice that
  when multiple model have the same name, some thing may go *wrong*.

  `!state` is a *atom* in which we'll save & recover the saved state.

  ```clojure
  {:model-name-0 {:prop-name-0 ...}
   :model-name-1 ...}
  ```
  "
  [model !state]
  (restore-model-inputs! model !state)
  (watch-model-inputs! model !state))
