(ns demo.list
  (:require
   [cljs.core.async :refer [go]]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]
   [tiltontec.cell.synapse :refer-macros [with-synapse]]))

(defn- list-prop [me k] (mx/mget (mx/fmu :list me) k))
(defn- list-prop! [me k v] (mx/mset! (mx/fmu :list me) k v))
(defn- list-prop-swap! [me k f & args] (apply mx/mswap! (mx/fmu :list me) k f args))

(def box-style {:border "1px solid #000" :padding "10px" :marginRight "10px"})

(defn- input-number? [evt]
  (let [v (-> evt .-target .-value)]
    (if (empty? v)
      0
      (try (parse-long v) (catch js/Error _ nil)))))

(defn kids-type-selection []
  (mxr/div {:style (assoc box-style :marginTop "8px" :marginBottom "8px")}
    (mxr/div {}
      (mxr/input {:type "checkbox"
                  :checked (mx/mget me :checked?)
                  :onChange (fn [e] (list-prop! me :raw? (not (-> e .-target .-checked))))}
        {:checked? (mx/cF (not (list-prop me :raw?)))})
      "use mx wrapped model as children?")
    (mxr/div {}
      (mxr/input {:type "checkbox"
                  :checked (mx/mget me :checked?)
                  :onChange (fn [e] (list-prop! me :raw? (-> e .-target .-checked)))}
        {:checked? (mx/cF (list-prop me :raw?))})
      "use raw react element as children?")))

(defn list-inputs []
  [(mxr/input {:style box-style
               :value (mx/mget me :c)
               :onChange #(some->> (input-number? %)
                                   (list-prop! me :count))}
     {:name :input :c (mx/cF (list-prop me :count))})
   (mxr/button {:style box-style :onClick #(list-prop-swap! me :count inc)} "+")
   (mxr/button {:style box-style :onClick #(list-prop-swap! me :count dec)} "-")])

(defn list-items [raw?]
  (if raw?
    (mxr/div {:style {:display "flex" :flexWrap "wrap"}}
      {:count (mx/cF (list-prop me :count))}
      (time (map (fn [v]
                   (mxr/$ :span {:style {:marginLeft "5px"} :key v}
                          "item" v))
                 (range (mx/mget me :count)))))
    (mxr/div
      {:style {:display "flex" :flexWrap "wrap"}}
      {:name :container

       ;; react to input asynchronously
       ;; :kid-values-watcher (mx/cF+
       ;;                       [:watch (mx/fn-watch
       ;;                                 (js/setTimeout
       ;;                                  #(mx/with-cc :watch
       ;;                                     (mx/mset! me :kid-values (range new)))
       ;;                                  0))]
       ;;                       (mx/mget (mx/mpar) :count))
       ;; :kid-values (mx/cI [])

       ;; react to input asynchronously using async cell
       :kid-values* (mx/cF+ [:async? true]
                      (let [c (mx/mget (mx/mpar) :count)]
                        (go (range c))))
       :kid-values (mx/cF (let [p me]
                            (with-synapse [:kids [prev (atom nil)]]
                              (or (when-some [v (mx/mget p :kid-values*)]
                                    (do (reset! prev v) v))
                                  @prev))))
       :kid-key #(mx/mget % :key)
       :kid-factory (fn [_me kid-val]
                      (mxr/span {:style {:marginLeft "5px"}}
                        {:key kid-val}
                        "item" kid-val))}
      (time (mx/kid-values-kids me _cache)))))

(defn MatrixApp []
  (mxr/div {} {:name :list :count (mx/cI 0) :raw? (mx/cI false)}
    (list-inputs)
    (kids-type-selection)
    (list-items (mx/mget me :raw?))))

(defn ReactApp
  []
  (let [[state set-state] (react/useState 10)]
    (react/createElement
     "div" #js {:style box-style}
     (react/createElement
      "input" #js {:style (clj->js box-style)
                   :value state
                   :onChange #(->> (input-number? %)
                                   (set-state))})
     (react/createElement
      "button" #js {:style (clj->js box-style)
                    :onClick #(set-state (inc state))} "+")
     (react/createElement
      "button" #js {:style (clj->js box-style)
                    :onClick #(set-state (dec state))} "-")
     (react/createElement
      "div" #js {:style #js {:display "flex" :flexWrap "wrap"}}
      (clj->js (for [i (range state)]
                 (react/createElement "span" #js {:key i :style #js {:marginLeft "5px"}}
                                      (str "item" i))))))))
