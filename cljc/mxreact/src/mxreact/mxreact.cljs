(ns mxreact.mxreact
  (:require-macros [mxreact.mxreact])
  (:require
   [react]
   [tiltontec.cell.base :refer [pulse-now unbound] :as mcb]
   [tiltontec.cell.poly :refer [md-quiesce md-quiesce-self watch-by-type]]
   [tiltontec.matrix.api :refer [fm-navig mget mget?] :as mx]
   [tiltontec.util.core]))

(defn ^js/React get-react [] react)

(def sid-latest (atom 0))
(def rendering-sid-latest (atom 0))

;; You can chooose to populate this with the root of your application's matrix.
(defonce matrix (atom nil))

(defn mxweb-init! []
  (reset! sid-latest 0)
  (reset! rendering-sid-latest 0))

;;; set-state functions
(def ssdict (atom {}))
(defn set-state-record [me setter]
  (swap! ssdict assoc (mget? me :sid) setter))
(defn set-state-unrecord [me]
  ;; (mx/prx :set-state-unrecord me)
  (when-some [sid (mget? me :sid)]
    (swap! ssdict dissoc sid)))

;;; refs
(def refdict (atom {}))
(defn ref-record [me ref]
  (swap! refdict assoc (mget me :sid) ref))
(defn ref-get [me]
  (get @refdict (mget me :sid)))
(defn ref-unrecord [me]
  (when-some [sid (mget? me :sid)]
    (swap! refdict dissoc sid)))

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
  (set-state-unrecord me)
  (ref-unrecord me)
  (doseq [k (:kids @me) :when (mx/any-ref? k)]
    (md-quiesce k))
  (md-quiesce-self me))

(defn state-hook-set! [me _slot]
  (if-let [sid (mget? me :sid)]
    (if-let [set-state-fn (get @ssdict sid)]
      (set-state-fn (pulse-now))
      (prn :shs-no-state-fn!!! sid (mget? me :name)))
    (prn :shs-no-sid!! (mget? me :name) me)))

(defmethod watch-by-type [:mxreact.mxreact/matrixrn.elt] [prop-name me _new-val old-val _c]
  ;; (mx/prx :obs-type-matrixrn-elt-entry prop-name me)
  (when (not= old-val unbound)
    ;; ^^^ observe forced anyway on new cells, when (= oldv unbound), so do not bother
    ;; (mx/prx :watch-by-type-setting-state prop-name (mget? me :name) (mget me :sid) (meta me) (mcb/mdead? me))
    (state-hook-set! me prop-name)))
