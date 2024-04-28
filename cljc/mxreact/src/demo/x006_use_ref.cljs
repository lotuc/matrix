(ns demo.x006-use-ref
  (:require
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]))

(defn ref-example0-component [^js/Object props]
  ;; This is a legistimate React component (a function takes props & returns
  ;; JSX)

  ;; mimics the https://react.dev/learn/manipulating-the-dom-with-refs#example-focusing-a-text-input
  (let [ref (react/useRef)]
    (mx/with-par (.-parentMx props)
      (-> (mxr/fragment {}
            (mxr/$ :input {:ref ref})
            (mxr/button {:onClick #(if-some [current (.-current ref)]
                                     (do (js/console.log current) (.focus current))
                                     (js/console.log "no ref?"))
                         :style {:marginLeft "1rem"}}
              "Focus the input (example0)"))
          (mxr/react-element)))))

(defn ref-example1 []
  (mxr/fragment {}
    (mxr/input {}
      ;; when use-ref? is true, we automatically add a `ref` props to current
      ;; model, the `ref` is created on component mounting and passed into
      ;; current component
      {:name :input :use-ref? true})
    (mxr/button {:onClick #(if-some [current (.-current (mx/mget me :input-ref))]
                             (.focus current)
                             (js/console.log "no ref?"))
                 :style {:marginLeft "1rem"}}
      {:input-ref (mx/cF (mx/mget (mx/fmu :input) :ref))}
      "Focus the input (example1)")))

(defn app []
  (mxr/div {}
    (mxr/mkc ref-example0-component {:parentMx me})
    (mxr/$ :br)
    (ref-example1)))
