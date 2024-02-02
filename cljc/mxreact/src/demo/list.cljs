(ns demo.list
  (:require
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]))

(def box-style {:border "1px solid #000" :padding "10px" :marginRight "10px"})

(defn MatrixApp []
  (mx/make ::list
    :rx-dom
    (mx/cFonce
      (mxr/div {} {:name :list :count (mx/cI 10)}
        (mxr/input {:style box-style
                    :value (mx/mget me :c)
                    :onChange #(when-some [n (try (parse-long (.-value (.-target %)))
                                                  (catch js/Error _))]
                                 (mx/mset! (mxr/fmu :list) :count n))}
          {:c (mx/cF (mx/mget (mx/fmu :list) :count))})
        (mxr/button {:style box-style
                     :onClick #(mx/mswap! (mxr/fmu :list) :count inc)} "+")
        (mxr/button {:style box-style
                     :onClick #(mx/mswap! (mxr/fmu :list) :count dec)} "-")
        (mxr/div
          {:style {:display "flex" :flexWrap "wrap"}}
          {:kid-values (mx/cF (doall (range (mx/mget (mx/fmu :list) :count))))
           :kid-key #(mx/mget % :key)
           :kid-factory (fn [_ kid-val]
                          (mxr/span {:style {:marginLeft "5px"}} {:key kid-val} "item" kid-val))}
          (mx/kid-values-kids me _cache))))))

(defn ReactApp
  []
  (let [[state set-state] (react/useState 10)]
    (react/createElement
     "div" #js {:style box-style}
     (react/createElement
      "input" #js {:style (clj->js box-style)
                   :value state
                   :onChange #(when-some [n (try (parse-long (.-value (.-target %)))
                                                 (catch js/Error _))]
                                (set-state n))})
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
