(ns tiltontec.cell.poly
  {:clj-kondo/ignore [:unused-binding]}
  (:require
   [tiltontec.util.core :refer [mx-type]]))

;;; --- life cycle -------------------

(defmulti c-awaken
  "You may not want to override this method, it's major implementation
  is in `evaluate` which implements `cell` and `c-formula`'s awaken
  behavior."
  mx-type)

(defmulti md-awaken-before
  "Being called just before model being awaken."
  mx-type)

(defmulti md-quiesce
  "`quiesce` behavior for given type. The default behavior in `evaluate`
  is just `quiesce` the model itself by calling `md-quiesce-self`. A
  more advanced behavior can be found `tiltontec.model.family`, it
  `quiesce` all the model's descendants and then call
  `md-quiesce-self` to quiesce itself."
  mx-type)

(defmulti md-quiesce-self
  "Do some actual `quiesce` behavior on the model itself."
  mx-type)

(defmethod md-awaken-before :default [me])

;;; --- change -----------------

(defmulti unchanged-test
  "Cells does not propagate when nothing changes. By default, the
  test is =, but cells can inject a different test, and when we get
  to models it will be possible for a prop to have associated
  with it a different test."
  (fn [me prop] [(mx-type me) prop]))

(defmethod unchanged-test :default [self propname] =)

;;; --- watch --------------------------

(defmulti watch
  "The watch multimethod is baked into cell's life cycle, it's default
  behavior is calling the `watch-by-type`."
  (fn [prop-name me new-val old-val c] [prop-name (mx-type me)]))

(defmulti watch-by-type
  "Notice that `watch-by-type` is just a default behavior of triggered
  by `watch`, meaning that if you override the `watch` for given model
  type, this method may not be called."
  (fn [prop-name me new-val old-val c] [(mx-type me)]))

;;; default `watch` & `watch-by-type` behaviors.

(defmethod watch :default [prop me new-val old-val c]
  (watch-by-type prop me new-val old-val c))

(defmethod watch-by-type :default [prop me new-val old-val c])
