(ns demo.x100-hello-world
  (:require
   ["react-slider" :default ReactSlider]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :refer-macros [mpar cF cFonce] :as mx]))

;; The JS:
;;    import ReactSlider from "react-slider";
;; Various solutions:
;;    fail ["react-slider" :refer [ReactSlider]]

(defn Slider []
  (mxr/mkc ReactSlider
           {}
           {:className "horizontal-slider"
            :thumbClassName "example-thumb"
            :defaultValue 42
            :trackClassName "example-track"}))

(defn app []
  (mx/make :mxreact/mxReactApp
           :rx-dom
           (cFonce
             ;; All DOM maker macros have the signature:
             ;;     (TAG mx-props react-props children*]
             ;;
             ;; TAG: div, span, button, et al
             ;; mx-props: reactive properties for the "host" Matrix object
             ;; react-props: props that will be passed to react/createElement
             ;; children: zero or more child Matrix objects
             ;;
            (mxr/div {} {}
                      ;; All the following arguments are children

                      ;; children are flattened and nils removed, so this nested array is no problem.
                     (for [msg ["Click the button, watch the title change. When you get to 42, the"
                                "the button will be disabled."]]
                       (mxr/p {} {} msg))

                     (mxr/button
                        ;; ----------------------------------------------------------------------
                        ;; This first group of props are for the Matrix "host" object.
                        ;; These can be moderated by reactive Cells, or just be assigned literals.
                        ;;
                      {;;
                         ;; cI is short for "input cell".
                         ;; We provide the starting value, then arbitrary imperative code can change it.
                         ;; The starting value form is evaluated immediately.
                       :counter   (mx/cI (+ 2 2))

                         ;; cF is short for "formulaic cell".
                         ;; The formula will be first evaluated as soon as this Button instance is added to the Matrix tree,
                         ;; then evaluated each time its dependencies change.
                         ;;
                         ;; Notes:
                         ;;   `mget` is a getter that establishes dependency;
                         ;;   'me' is an anaphor (established by the macro cF) akin to 'self' or 'this';
                         ;;   the formula can be arbitrarily complexe CLJ code.
                       :title     (cF (str "Click me to count higher: " (mx/mget me :counter)))

                       :tx-color (cF (if (even? (mx/mget me :counter))
                                       "red" "green"))

                         ;; do not count too high! There is no turning back once disabled, until we had
                         ;; a countdown button in the next lesson.
                       :disabled? (cF (= 42 (mx/mget me :counter)))

                         ;; TryThis[times-six,avg]: counting to 42 (to test the above) takes a lot of clicking!
                         ;; Add a new computed ":times-six" property that is six times the :counter property,
                         ;; and have :disabled? watch for /that/ property to get to 42.
                       }
                        ;; --------------------------------------------------------------
                        ;; React element props: these props are passed to createElement.
                        ;; The map will be (deeply) converted to JS by clj->js.
                        ;; Note: We can pull properties from 'me' (or elsewhere), but not use formulas here.
                      {:disabled (mx/mget me :disabled?)
                         ;; ^^^ soon we will discover the macro `with-props` to make the above less boilerplate-y

                       :style    {:color (mx/mget me :tx-color)}
                         ;; Below: 'me' is bound to an atom holding an MX map/instance
                         ;; so we use a swap! wrapper that adds reactive notification
                         ;; to modify the counter. We could just `swap!`, but then no dataflow
                       :onClick  #(mx/mswap! me :counter inc)}

                        ;; reactive text content
                      (mxr/mx$ (mx/mget (mpar me) :title)))

                      ;; plain matrix model, won't be rendered as react element
                     (mx/make ::router
                              :name :router
                              :hash (mx/cI js/window.location.hash
                                            ;; watch the cell changes
                                           :watch (fn [_prop _me new _old _c]
                                                    (when-not (= new js/window.location.hash)
                                                      (set! js/window.location.hash new)))))

                      ;; plain string/number or expression that creates it
                     42 "+" 42 "=" (+ 42 42)
                     (mxr/br {} {})

                      ;; build reactive text with mx$
                     "location.hash: " (mxr/mx$
                                         ;; find model by name
                                        (let [router-model (mxr/fm* me :router)]
                                           ;; Notes:
                                           ;;   `mget` is a getter that establishes dependency
                                          (mx/mget router-model :hash)))
                     ", click " (for [href ["#/42" "#/24"]]
                                  (mxr/a {} {:style {:marginRight "5px" :borderBottom "3px solid red" :cursor "pointer"}
                                             :onClick (fn [_] (mx/mset! (mxr/fm* me :router) :hash href))}
                                         href))
                     "to change."

                      ;; the slider component
                     (Slider)))))
