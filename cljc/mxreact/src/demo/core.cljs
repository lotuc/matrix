(ns demo.core
  (:require
   ["react-dom/client" :refer [createRoot]]
   [demo.list]
   [demo.reagent-interop :as reagent-interop]
   [demo.todomvc :as todomvc]
   [demo.x100-hello-world :as x100-hello-world]
   [mxreact.mxreact :refer-macros [fm*] :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]))

(set! *print-level* 100)

(defonce app-dom-node (js/document.getElementById "app"))
(defonce app-react-root (createRoot app-dom-node))

(defn render-root-element
  "https://react.dev/reference/react-dom/client/createRoot"
  [e]
  (.render app-react-root e))

(defn render-matrix-app [app]
  (mxr/mxreact-init!)
  (->> (mxr/make-mxreact-app app)
       (reset! mxr/matrix)
       (mxr/mxreact-app-root-element)
       (render-root-element)))

(comment
  ;; find model with fm*
  (def todo-list (fm* :todo-list (:rx-dom @@mxr/prev-matrix)))
  ;; read model's prop with mget
  (mx/mget todo-list :value)
  ;; set! model's prop with mset!
  (mx/mset! todo-list :value [])
  (mx/mset! (fm* :todo-list (:rx-dom @@mxr/matrix))
    :value (map (fn [i] {:id i :title (str "item" i)
                         :completed? false})
                (range 10))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load start []
  (comment
    (render-matrix-app x100-hello-world/app)

    (render-matrix-app todomvc/TodoMVC)

    (render-matrix-app demo.list/MatrixApp)
    (render-root-element (mxr/$ demo.list/ReactApp))

    (render-root-element (reagent-interop/reagent-app))
    (render-matrix-app reagent-interop/MatrixApp))

  (render-matrix-app x100-hello-world/app))
