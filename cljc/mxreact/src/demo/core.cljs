(ns demo.core
  (:require
   ["react-dom/client" :refer [createRoot]]
   [demo.x001-hello-world]
   [demo.x002-child]
   [demo.x003-todomvc]
   [demo.x004-reagent-interop]
   [demo.x005-list]
   [demo.x006-use-ref]
   [demo.x007-async]
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
  (render-matrix-app demo.x007-async/app))
