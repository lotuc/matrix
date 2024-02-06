(ns mxreact.mxreact
  (:require-macros [mxreact.mxreact])
  (:require
   [react]
   [tiltontec.cell.base :refer [pulse-now unbound] :as mcb]
   [tiltontec.cell.poly :refer [md-quiesce md-quiesce-self watch-by-type]]
   [tiltontec.matrix.api :refer [mget?] :as mx]))

(defn ^js/React get-react [] react)

(def sid-latest (atom 0))

;;; You can chooose to populate this with the root of your application's matrix.
;;; `mxweb-init!` will quiesce the matrix and reset it to `nil` (this is useful
;;; at developing time)
(defonce matrix (atom nil))

(defn make-mxreact-app [app]
  (mx/make :mxreact.mxreact/mxreact.app
    :rx-dom (mx/cFonce (mx/with-par me (app)))))

(defn mxreact-app-root-element [app]
  (when-some [rx-dom (mx/mget app :rx-dom)]
    (mx/mget? rx-dom :react-element)))

(defn mxreact-init! []
  (when-some [m (first (reset-vals! matrix nil))]
    (when-not (isa? (mx/mx-type m) :mxreact.mxreact/mxreact.app)
      (js/console.error "mxreact-init! called with non-mxreact app" m))
    (md-quiesce m))
  (reset! sid-latest 0))

(defmethod md-quiesce :mxreact.mxreact/mxreact.app [me]
  (js/setTimeout
   (fn []
     ;; cleanup asynchonously
     (when-some [rx-dom (mx/mget? me :rx-dom)]
       (md-quiesce rx-dom))
     (md-quiesce-self me))
   0))

(derive :mxreact.mxreact/mxreact.elt :tiltontec.model/family)

(defmethod md-quiesce :mxreact.mxreact/mxreact.elt [me]
  ;; normally called by kids observer, but we shadow that
  (js/setTimeout
   (fn []
     ;; cleanup asynchonously
     (doseq [k (:kids @me) :when (mx/any-ref? k)]
       (md-quiesce k))
     (md-quiesce-self me))
   0))

(defn state-hook-set! [me _slot]
  (let [set-state-fn (mget? me :set-state-fn ::not-found)]
    (cond
      (= set-state-fn ::quiesce)
      (js/console.warn "model quiesced" me)

      (= set-state-fn ::not-found)
      (js/console.warn "no :set-state-fn attached to: " me)

      set-state-fn
      (set-state-fn (pulse-now)))))

(defmethod watch-by-type
  [:mxreact.mxreact/mxreact.elt] [prop-name me _new-val old-val _c]
  (when (and
          (not= old-val unbound)
          ;; ^^^ observe forced anyway on new cells, when (= oldv unbound), so
          ;; do not bother
          (not (#{:set-state-fn :ref} prop-name))
          ;; ^^^ these fields are initialized by the react component which
          ;; should not cause rerendering
          )
    (state-hook-set! me prop-name)))
