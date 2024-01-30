(ns mxreact.mxreact
  (:refer-clojure :exclude [map meta time])
  (:require
   [tiltontec.matrix.api :as mx]))

(defmacro $
  [type & args]
  (let [type (if (keyword? type) (name type) type)]
    (cond
      (map? (first args))
      `(.createElement (mxreact.mxreact/get-react) ~type (cljs.core/clj->js ~(first args)) ~@(rest args))
      :else
      `(.createElement (mxreact.mxreact/get-react) ~type nil ~@args))))

(defmacro with-props [[& inherited] static-props]
  `(merge (into {} (for [prop# [~@inherited]]
                     (let [[pkey# pget#] (if (vector? prop#)
                                           prop#
                                           [prop# prop#])]
                       [pkey# (tiltontec.matrix.api/mget ~'me pget#)])))
          ~static-props))

(defmacro component-with-hooks [& body]
  `(fn []
     (let [[state# set-state#] (.useState (mxreact.mxreact/get-react) 0)]
       (.useEffect (mxreact.mxreact/get-react)
                   (fn []
                     (some->> (when (tiltontec.matrix.api/mget? ~'me :use-ref?)
                                (.useRef (mxreact.mxreact/get-react) :ref-undefined))
                              (mxreact.mxreact/ref-record ~'me))
                     (mxreact.mxreact/set-state-record ~'me set-state#)
                     (fn []
                       (tiltontec.cell.poly/md-quiesce ~'me)))
                   (cljs.core/clj->js [~'me]))
       ~@body)))

(defmacro mx$ [textFormulaBody]
  ;; we create Text with a string child, but one potentially reactive
  (let [content-kwd (keyword (gensym "content"))]
    `(tiltontec.matrix.api/make
      :mxreact.mxreact/matrixrn.elt
      :name (gensym "strng")
      :sid (swap! mxreact.mxreact/sid-latest inc)
      ~content-kwd (tiltontec.matrix.api/cF ~textFormulaBody)
      :react-element (tiltontec.matrix.api/cF
                      (.createElement
                       (mxreact.mxreact/get-react)
                       (mxreact.mxreact/component-with-hooks
                        (.createElement
                         (mxreact.mxreact/get-react) "span"
                         (cljs.core/clj->js {:key (rand-int 9999)}) {}
                         (tiltontec.matrix.api/mget ~'me ~content-kwd))))))))

(defn mk-react-element-with-kids [react-component jsx-props]
  `(tiltontec.matrix.api/cF
    (.createElement (mxreact.mxreact/get-react)
                    (mxreact.mxreact/component-with-hooks
                     (apply react/createElement ~react-component
                            (cljs.core/clj->js
                             (cond-> ~jsx-props
                               (tiltontec.matrix.api/mget? ~'me :use-ref?)
                               (assoc :ref (mxreact.mxreact/ref-get ~'me))))
                            (->> (tiltontec.matrix.api/mget? ~'me :kids)
                                 (clojure.core/map
                                  (fn [mapkid#]
                                    (when mapkid#
                                      (if (tiltontec.matrix.api/any-ref? mapkid#)
                                        (tiltontec.matrix.api/mget? mapkid# :react-element)
                                        (str mapkid#)))))
                                 (clojure.core/filter some?)))))))

(defmacro mkc [react-component jsx-props & kids]
  (let [[mx-props kids] (if (map? (first kids))
                          [(first kids) (rest kids)]
                          [{} kids])]
    `(tiltontec.matrix.api/make
      :mxreact.mxreact/matrixrn.elt
      :sid (swap! mxreact.mxreact/sid-latest inc)
      ~@(when (seq kids) `(:kids (tiltontec.matrix.api/cFkids ~@kids)))
      :react-element ~(mk-react-element-with-kids react-component jsx-props)
      ~@(apply concat (into [] mx-props)))))

(defmacro mk [node-type jsx-props & kids]
  (let [[mx-props kids] (if (map? (first kids))
                          [(first kids) (rest kids)]
                          [{} kids])]
    `(tiltontec.matrix.api/make
      :mxreact.mxreact/matrixrn.elt
      :sid (swap! mxreact.mxreact/sid-latest inc)
      ~@(when (seq kids) `(:kids (tiltontec.matrix.api/cFkids ~@kids)))
      :react-element ~(mk-react-element-with-kids `(name ~node-type) jsx-props)
      ~@(apply concat (into [] mx-props)))))

(declare
 input textarea option select a abbr address area article aside audio b base bdi
 bdo big blockquote body br button canvas caption cite code col colgroup data datalist
 dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form
 h1 h2 h3 h4 h5 h6 head header hr html i iframe img ins kbd keygen label legend li link
 main map mark menu menuitem meta meter nav noscript object ol optgroup output p param
 picture pre progress q rp rt ruby s samp script section small source span strong style
 sub summary sup table tbody td tfoot th thead time title tr track u ul var video wbr
 circle clipPath ellipse g line mask path pattern polyline rect svg text defs
 linearGradient polygon radialGradient stop tspan)

(def tags
  '[input textarea option select a abbr address area article aside audio b base bdi
    bdo big blockquote body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption figure footer form
    h1 h2 h3 h4 h5 h6 head header hr html i iframe img ins kbd keygen label legend li link
    main map mark menu menuitem meta meter nav noscript object ol optgroup output p param
    picture pre progress q rp rt ruby s samp script section small source span strong style
    sub summary sup table tbody td tfoot th thead time title tr track u ul var video wbr
    circle clipPath ellipse g line mask path pattern polyline rect svg text defs
    linearGradient polygon radialGradient stop tspan])

(comment
  ;; generating clj-kondo hook configuration for tags
  (doseq [tag tags]
    (println (str "mxreact.mxreact/" tag " hooks.mxreact.mxreact/tag"))))

(defn gen-tag
  [tag]
  `(defmacro ~tag [& args#]
     `(mk ~(str '~tag) ~@args#)))

(defmacro gen-tags []
  `(do
     ~@(for [tag tags]
         (gen-tag tag))))

(gen-tags)

(defmacro fmu [what]
  `(tiltontec.matrix.api/fm-navig ~what ~'me
                                  :me? false
                                  :inside? false
                                  :must? true
                                  :up? true))

(defmacro fmu-val [what prop]
  `(tiltontec.matrix.api/mget (mxreact.mxreact/fmu ~what) ~prop))

(defmacro fmi [what]
  `(tiltontec.matrix.api/fm-navig ~what ~'me
                                  :me? true
                                  :inside? true
                                  :must? true
                                  :up? false))

(defmacro fmi-val [what prop]
  `(tiltontec.matrix.api/mget (mxreact.mxreact/fmi ~what) ~prop))

(defmacro myval [prop]
  `(tiltontec.matrix.api/mget ~'me ~prop))
