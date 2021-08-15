(ns myapp.build
  (:require [cljs.pprint :as pp]
            [cljs-time.coerce :as tmc]
            [clojure.string :as str]
    ;[bide.core :as r]
            [taoensso.tufte :as tufte :refer-macros (defnp profiled profile)]

            [tiltontec.util.core :refer [pln xor now]]
            [tiltontec.cell.base
             :refer-macros [without-c-dependency]
             :refer [unbound ia-type *within-integrity* *defer-changes*]]
            [tiltontec.cell.core :refer-macros [cF cF+ cFn cF+n cFonce] :refer [cI]]
            [tiltontec.cell.observer :refer-macros [fn-obs]]


            [tiltontec.model.core
             :refer-macros [cFkids with-par]
             :refer [matrix mx-par mget mset! mswap!
                     fget mxi-find mxu-find-type
                     kid-values-kids] :as md]

    ;[goog.events.Event :as event]
    ;[goog.dom.forms :as form]

            ["react-native" :as rn]
            [helix.core :as hx :refer [defnc $ <>]]

    ;;[helix.dom :as d]
            [helix.hooks :as hooks]

            [myapp.mxreact :as mxr :refer [mkrx]]
            [myapp.mxrgen :refer-macros [mkbox mkx mxfnc]]
            ))

(declare mx-find-matrix)

;; (mkrx
;                          {:name      :root
;                           :rendering (cF (mxfnc
;                                            (apply $ rn/View
;                                              {:style (clj->js {:flex 1
;                                                           :alignItems "center"
;                                                           :justifyContent "center"
;                                                           :backgroundColor "white"})}
;                                              {}
;                                              (doall (map #(mget % :rendering)
;                                                       (mget me :kids))))))}
;                          {}
;                          (cFkids

(defn matrix-build! []
  (reset! mxr/ssdict {})
  (reset! mxr/refdict {})
  (reset! matrix
    (md/make ::hxApp
      :rx-dom (cFonce (with-par me
                        (mkbox rn/View
                          :name :root
                          :style (clj->js {:flex            1
                                           :marginTop       96
                                           :marginLeft      96
                                           :padding         24
                                           :alignItems      "flex-start"
                                           ;;:justifyContent  "center"
                                           :backgroundColor "coral"})
                          :of-kids
                          (vector
                            (mkx rn/Button
                                    :name :counter42
                                    :title (cF (str "Booomer " (mget me :counter)))
                                    :counter (cI 3)
                                    :jsx {:title   (mget me :title)
                                          :onPress #(mswap! me :counter inc)})
                            (mkbox rn/View
                              :style (js-obj "backgroundColor" "yellow")
                              :of-kids (for [n (range (mget (mxr/mxu! me :counter42) :counter))]
                                         (mkrx
                                           {:rendering (cF ($ rn/Text {} {}
                                                             (str "Text " n)))}))))))))))


;;;
#_(mx/button
    :name :counter42
    :title (cF (str "Bumper " (mget me :counter)))
    :counter (cI 3)
    :jsx (with-props [:title :title]                        ;; {... :title (mget me :title ... }
           {:onPress #(mswap! me :counter inc)})

    #_(mx/view {:style (js-obj "backgroundColor" "yellow")}
        (for [n (range (mget (mxr/mxu! me :counter42) :counter))]
          (mktext (str "Text " n))))
    )



#_(defn mx-find-matrix [mx]
    (mxu-find-type mx ::hxApp))
