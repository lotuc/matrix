(ns myapp.mxreact
  (:refer-clojure :exclude [meta time])
  (:require
    [helix.hooks :as hooks]
    [goog.dom.forms :as form]
    [cljs.pprint :refer [pprint cl-format]]
    [tiltontec.cell.base :refer [md-ref? ia-type unbound c-pulse pulse-now]]
    [tiltontec.cell.evaluate :refer [not-to-be not-to-be-self]]
    [tiltontec.cell.observer :refer [observe observe-by-type]]
    [tiltontec.cell.core :refer-macros [cF cF+ cFn cF+n cFonce] :refer [cI]]
    [tiltontec.model.core
     :refer-macros [cFkids with-par]
     :refer [make matrix mx-par mget mset! mswap!
             fget mxi-find mxu-find-type
             kid-values-kids] :as md]
    [helix.core :as hx :refer [$ <>]]))

(def +tag-sid+ (atom -1))

(defn mxweb-init! []
  (reset! +tag-sid+ -1))

(def tag-by-id (atom {}))

(def ssdict (atom {}))
(defn set-state-record [me setter]
  (swap! ssdict assoc (mget me :sid) setter))

(def refdict (atom {}))
(defn ref-record [me ref]
  (swap! refdict assoc (mget me :sid) ref))

(def sid-latest (atom 0))

(defn mxu! [me name]
  (fget name me
    :me? false
    :inside? false
    :must? true
    :up? true))

(defn mx*
  ([me name] (mx* me name true))
  ([me name must-find?]
   (fget name me
     :me? true
     :inside? true
     :up? true
     :must? must-find?)))

(defn make-rnc [tag attrs aux cFkids]
   (prn :make-rnc1 tag attrs cFkids)
   (let [tag-id (str (or (:id attrs)
                       (str tag "-" (swap! +tag-sid+ inc)))) ;; todo GUID
         rest-kvs (concat (vec (apply concat (seq (dissoc attrs :id))))
                    (vec (apply concat (seq aux))))
         ;; _ (prn :make-rnc-rkvs!! (count rest-kvs) rest-kvs)
         #_(prn :aux-raw aux)
         #_(prn :addl-slots (concat (vec (apply concat (seq (dissoc attrs :id))))
                              (vec (apply concat (seq aux)))))
         mx-tag (apply make ::mxrn.elt
                  :tag tag
                  :id tag-id
                  :sid (swap! sid-latest inc)
                  :attr-keys (distinct (conj (keys attrs) :id))
                  :kids cFkids
                  :rendering (cF (apply $ (mget me :tag) attrs
                                   ;; todo where is the useState?
                                   (or (when-let [c (mget me :content)]
                                         [c])
                                     (let [kids (mget me :kids)]
                                       #_ (prn :rnc-kid-render! (map #(mget % :rendering)
                                                               (mget me :kids)))
                                       (doall (map #(mget % :rendering)
                                                (mget me :kids)))))))
                  rest-kvs)]
     (swap! tag-by-id assoc tag-id mx-tag)
     mx-tag))

(defn mkrx
  ;; todo lose vstg/tag altogether
  ([attributes]
   (prn :mkrx-1!!!!!!!!!)
   (make-rnc "vstg" attributes {} nil))
  ([attributes aux]
   (prn :mkrx-2!!!!!!!!!!!!!)
   (make-rnc "vstg" attributes aux nil))
  ([attrs aux cFkids]
   (prn :mkrx-3!!!!!!!!!!! attrs aux)
   (let [tag-id (str (or (:id attrs)
                       (str "vstg" "-" (swap! +tag-sid+ inc)))) ;; todo GUID
         rest-kvs (concat (vec (apply concat (seq (dissoc attrs :id))))
                    (vec (apply concat (seq aux))))
         ;; _ (prn :mkrx-sees (count rest-kvs) rest-kvs)
         mx-tag (apply make ::mxrn.elt
                  :tag "vstg"
                  :id tag-id
                  :sid (swap! sid-latest inc)
                  :attr-keys (distinct (conj (keys attrs) :id))
                  :kids cFkids
                  rest-kvs)]
     ;;(prn :mkrx-built mx-tag)
     (swap! tag-by-id assoc tag-id mx-tag)
     mx-tag)))

(defmethod not-to-be [::mxrn.elt] [me]
  ;; todo: worry about leaks
  (doseq [k (:kids @me)]
    (when (md-ref? k)
      (not-to-be k)))
  ;; todo extend these APIs for cleanup
  (swap! tag-by-id dissoc (mget me :id))
  (swap! tag-by-id dissoc (mget me :ref))
  (not-to-be-self me))

(defmethod observe-by-type [::mxrn.elt] [slot me newv oldv cell]
  (when (not= oldv unbound)                                 ;; observe forced anyway on new cells
    ;;(prn :obs????????? (mget me :name)(mget me :sid)(mget me :id))
    (when-let [set-state-fn (get @ssdict (mget me :sid))]
      ;;(prn :obs!!!!!!!!! (mget me :name)(mget me :sid))
      (set-state-fn (pulse-now)))))