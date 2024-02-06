(ns demo.reagent-interop
  (:require
   [mxreact.mxreact :as mxr]
   [react]
   [reagent.core :as r]
   [tiltontec.matrix.api :as mx]))

(def click-count (r/atom 0))

(defn count-color [v]
  (when v (if (even? v) "red" "green")))

(defn Counting
  [{:keys [count-color]} & children]
  ;; Nesting component
  ;; https://github.com/reagent-project/reagent/issues/68

  [:div {:style {:border "1px solid green"
                 :padding "0px 10px 10px 10px"}}
   [:p {:style {:fontWeight "400"}}
    [:a {:target "_blank" :href "https://github.com/reagent-project/reagent"} "Reagent"]
    " hiccup component"]
   [:div {:style {:color count-color}}
    "The click-count atom was clicked " @click-count " times. "
    [:button {:on-click #(swap! click-count inc)
              :style {:border "1px solid #000" :padding "0 8px"}}
     "Click me!"]]
   (when children
     [:div {:style {:border "1px solid green" :marginTop "10px" :padding "10px"}}
      [:span children]])])

(defn MxCounting []
  (let [fc (fn [{:keys [mx-model] :as props}]
             ;; We can use useEffect to setup the reagent atom listen.
             (react/useEffect
              (fn []
                (->> #(when mx-model (mx/mset! mx-model :count %4))
                     (add-watch click-count ::watch))
                #(remove-watch click-count ::watch))
              #js [mx-model])
             [Counting props (:children props)])
        ;; Essential for functional component that uses hooks.
        ;; https://github.com/reagent-project/reagent/blob/master/doc/ReactFeatures.md#hooks
        fc (fn [props] [:f> fc props])]
    (mxr/mk-reagent
      fc
      {:count-color (mx/mget me :count-color)
       ;; Passing model into reagent component, we setup the reagent atom listen
       ;; there with useEffect. Note that if we don't pass the model in, we can
       ;; also lookup the model via app root (checkout `demo.core/matrix-build!`).
       :mx-model me}
      {:name :reagent-counting
       :count (mx/cI nil)
       :count-color (mx/cF (count-color (mx/mget me :count)))}

     ;; The following children will be passing into the reagent component.
      (mxr/p {:style {:fontWeight "400"}} "mxreact children")
      (mxr/p {} "not reactive to reagent atom: " @click-count)
      (mxr/p {:style {:color (mx/mget me :color)}}
        {:color (mx/cF (mx/mget (mx/fmu :reagent-counting) :count-color))}
        "reactive to mx model: " (mx/mget (mx/fmu :reagent-counting) :count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matrix APP as top level & interact with reagent components
(defn MatrixApp []
  (mxr/div {}
    (mxr/p {:style {:fontSize "1.2rem"}}
      "Interop with Reagent")
    (MxCounting)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reagent APP as top level & interact with matrix apps

(defonce matrix-app-ctx (react/createContext "default"))
(def mx-app-provider (.-Provider matrix-app-ctx))
(def mx-app-consumer (.-Consumer matrix-app-ctx))

(defn MatrixComponent []
  ;; retrieve the matrix app from the context provider and render it.
  [:> mx-app-consumer {}
   (fn [matrix-app]
     (mx/mget (mx/mget matrix-app :rx-dom) :react-element))])

(defn ReagentApp []
  [:div
   [:p {:style {:fontSize "1.2rem"}}
    "Interop with Matrix"]
   [Counting {:count-color (count-color @click-count)}]
   [:p "The following component is rendered by Matrix"]
   [:div {:style {:padding "10px" :border "1px solid #000"}}
    [:f> MatrixComponent {:key 1}]]])

(defn reagent-app []
  ;; build your matrix app, put it into a context provider.
  (let [matrix-app (MatrixApp)]
    (r/as-element
     [:> mx-app-provider {:value matrix-app}
      [ReagentApp]])))
