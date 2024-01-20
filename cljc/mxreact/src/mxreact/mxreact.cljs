(ns mxreact.mxreact
  (:require-macros [mxreact.mxreact])
  (:require
   [react]
   [tiltontec.cell.base :refer [pulse-now unbound]]
   [tiltontec.cell.poly :refer [watch-by-type]]
   [tiltontec.matrix.api :refer [fm-navig mget]]))

;(def <> react/createElement)

(defn ^js/React get-react [] react)

(def sid-latest (atom 0))
(def rendering-sid-latest (atom 0))

(defn mxweb-init! []
  (reset! sid-latest 0)
  (reset! rendering-sid-latest 0))

(def ssdict (atom {}))
(defn set-state-record [me setter]
  ;;(prn :recording-new-set-state (mget me :sid))
  (swap! ssdict assoc (mget me :sid) setter))
(defn set-state-unrecord [me]
  ;;(prn :setstate-unrecord (mget me :sid))
  (swap! ssdict dissoc (mget me :sid)))

(def refdict (atom {}))
(defn ref-record [me ref]
  (swap! refdict assoc (mget me :sid) ref))
(defn ref-get [me]
  (get @refdict (mget me :sid)))
(defn ref-unrecord [me]
  ;;(prn :refunrecord (mget me :sid))
  (swap! refdict dissoc (mget me :sid)))

(defn fm*
  ([me name] (fm* me name true))
  ([me name must-find?]
   (fm-navig name me
             :me? true
             :inside? true
             :up? true
             :must? must-find?)))

;; (defmethod not-to-be [:mxreact.mxreact/matrixrn.elt] [me]
;;   ;; normally called by kids observer, but we shadow that
;;   ;;(prn :not-to-be-entry!!!! me)
;;   (set-state-unrecord me)
;;   (ref-unrecord me)
;;   (doseq [k (:kids @me)]
;;     (when (md-ref? k)
;;       (not-to-be k)))
;;   (not-to-be-self me))

(defn state-hook-set! [me _slot]
  (if-let [sid (mget me :sid)]
    (if-let [set-state-fn (get @ssdict sid)]
      (set-state-fn (pulse-now))
      (prn :shs-no-state-fn!!! (mget me :name) sid))
    (prn :shs-no-sid!! (mget me :name) me)))

(defmethod watch-by-type [:mxreact.mxreact/matrixrn.elt] [slot me _newv oldv _cell]
  ;;(prn :obs-type-matrixrn-elt-entry??? slot (mget me :name)(mget me :sid))
  (when (not= oldv unbound)
    ;; ^^^ observe forced anyway on new cells, when (= oldv unbound), so do not bother
    ;; (prn :obs-by-type-setting-state slot (mget me :name) (mget me :sid) #_(meta me) (mdead? me))
    (state-hook-set! me slot)))
