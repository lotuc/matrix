(ns mxreact.mxreact
  (:require-macros [mxreact.mxreact])
  (:require
   [react]
   [tiltontec.cell.base :refer [pulse-now unbound] :as mcb]
   [tiltontec.cell.poly :refer [md-quiesce md-quiesce-self watch-by-type]]
   [tiltontec.matrix.api :refer [fm-navig mget?] :as mx]
   [tiltontec.util.core]))

(defn ^js/React get-react [] react)

(def sid-latest (atom 0))

;; You can chooose to populate this with the root of your application's matrix.
(defonce matrix (atom nil))

(defn mxweb-init! []
  (reset! sid-latest 0))

(defn fm*
  ([me name] (fm* me name true))
  ([me name must-find?]
   (fm-navig name me
     :me? true
     :inside? true
     :up? true
     :must? must-find?)))

(defmethod md-quiesce :mxreact.mxreact/matrixrn.elt [me]
  ;; normally called by kids observer, but we shadow that
  (doseq [k (:kids @me) :when (mx/any-ref? k)]
    (md-quiesce k))
  (md-quiesce-self me))

(defn state-hook-set! [me _slot]
  (let [set-state-fn (mget? me :set-state-fn ::not-found)]
    (cond
      (= set-state-fn ::not-found)
      (js/console.warn "no :set-state-fn attached to: " me)

      set-state-fn
      (set-state-fn (pulse-now)))))

(defmethod watch-by-type
  [:mxreact.mxreact/matrixrn.elt] [prop-name me _new-val old-val _c]
  (when (and
          (not= old-val unbound)
          ;; ^^^ observe forced anyway on new cells, when (= oldv unbound), so
          ;; do not bother
          (not (#{:set-state-fn :ref} prop-name))
          ;; ^^^ these fields are initialized by the react component which
          ;; should not cause rerendering
          )
    (state-hook-set! me prop-name)))
