(ns demo.core
  (:require
   ["react-dom/client" :refer [createRoot]]
   [demo.slider :as slider]
   [demo.todo-mvc :as todo-mvc]
   [demo.x100-hello-world :as x100-hello-world]
   [mxreact.mxreact :as mxr]
   [react]
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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load start []
  (comment
    (render-root-element (mxr/$ demo.slider/App))
    (render-matrix-app todo-mvc/demo)
    (render-matrix-app x100-hello-world/app))

  (render-matrix-app x100-hello-world/app))
