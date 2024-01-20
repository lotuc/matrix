(ns demo.slider
  (:require
   ["react-slider" :default ReactSlider]
   [react]))

;;; pure React component interop

(defn App
  []
  (react/createElement
   ReactSlider #js {:className "horizontal-slider"
                    :defaultValue 42
                    :thumbClassName "example-thumb"
                    :trackClassName "example-track"}))
