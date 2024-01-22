(ns demo.core
  (:require
   ["react-dom/client" :refer [createRoot]]
   [demo.reloaded :as reloaded]
   [demo.slider :as slider]
   [demo.todomvc :as todomvc]
   [demo.x100-hello-world :as x100-hello-world]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]
   [tiltontec.model.core :as md]))

(defonce app-dom-node (js/document.getElementById "app"))
(defonce app-react-root (createRoot app-dom-node))

(defn render-root-element
  "https://react.dev/reference/react-dom/client/createRoot"
  [e]
  (.render app-react-root e))

(defn matrix-build! [app]
  (reset! mxr/ssdict {})
  (reset! md/matrix (app)))

(defn render-matrix-app [app]
  (let [app-matrix (matrix-build! app)
        root-mx (md/mget app-matrix :rx-dom)
        root-element (md/mget root-mx :react-element)]
    (render-root-element root-element)))

(comment
  ;; find model with fm*
  (def todo-list (mxr/fm* (:rx-dom @@md/matrix) :todo-list))
  ;; read model's prop with mget
  (mx/mget todo-list :value)
  ;; set! model's prop with mset!
  (mx/mset! todo-list :value []))

(defonce !state (atom nil))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load start []
  (comment
    (render-root-element (mxr/$ demo.slider/App))
    (render-matrix-app todomvc/demo)
    (render-matrix-app x100-hello-world/app))

  (render-matrix-app todomvc/demo)
  ;; (render-root-element (mxr/$ demo.slider/App))

  ;; `render-matrix-app` saves root app's model in md/matrix
  (when-some [m (some-> md/matrix deref deref :rx-dom)]
    ;; save some state between code reload
    (reloaded/restore-and-watch-model-inputs! m !state)))
