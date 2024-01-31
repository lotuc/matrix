(ns demo.core
  (:require
   ["react-dom/client" :refer [createRoot]]
   [demo.todomvc :as todomvc]
   [demo.list]
   [demo.x100-hello-world :as x100-hello-world]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]
   [tiltontec.model.core :as md]
   [demo.reagent-interop :as reagent-interop]))

(set! *print-level* 3)

(defonce app-dom-node (js/document.getElementById "app"))
(defonce app-react-root (createRoot app-dom-node))

(defn render-root-element
  "https://react.dev/reference/react-dom/client/createRoot"
  [e]
  (.render app-react-root e))

(defn matrix-build! [app]
  (reset! mxr/ssdict {})
  (reset! mxr/matrix (app)))

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
  (mx/mset! todo-list :value [])
  (mx/mset! (mxr/fm* (:rx-dom @@md/matrix) :todo-list)
            :value (map (fn [i] {:id i :title (str "item" i)
                                 :completed? false})
                        (range 10))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load start []
  (comment
    (render-matrix-app todomvc/TodoMVC)
    (render-matrix-app x100-hello-world/app)

    (render-matrix-app demo.list/MatrixApp)
    (render-root-element (mxr/$ demo.list/ReactApp))

    (render-root-element (reagent-interop/reagent-app))
    (render-matrix-app reagent-interop/MatrixApp))

  (render-matrix-app x100-hello-world/app))
