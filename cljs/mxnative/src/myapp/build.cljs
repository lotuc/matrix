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

            [myapp.mxreact :as mxr]
            ))

(declare mx-find-matrix)

#_(defnc Title3
    [props]
    (let [[_ ss] (hooks/use-state 0)
          ref (hooks/use-ref nil)]
      (mxr/set-state-record (:me props) ss)
      (mxr/ref-record (:me props) ref)
      (d/h1 {:ref ref :on-click (:on-click props)} {}
        (str "Todos " (:mas props) ":" (rand-int 32767) ":count=" (mget (:me props) :counter)))))

(defn matrix-build! []
  (reset! mxr/ssdict {})
  (reset! mxr/refdict {})
  (reset! matrix (md/make ::hxApp
                   :rx-dom (cFonce (with-par me
                                     ;; OK (mxr/make-rnc "p" {} {:content "Just P"} nil)
                                     ;;{:style #js {:fontSize 36}}
                                     #_(mxr/make-rnc-ex "text"
                                         {:name      :counter42
                                          :content   "baby steps" ;; (cF (str "Boom " (mget me :counter)))
                                          :rendering (cF (hx/fnc []
                                                           (let [[_ set-state] (hooks/use-state 0)]
                                                             (mxr/set-state-record me set-state)
                                                             (apply $ rn/Text
                                                               {:style #js {:fontSize 36}} {} "xx BAM BAM text"))))}
                                         {})

                                     (mxr/make-rnc-ex "dummy"
                                       {:name      :root
                                        :rendering (cF (do #_$
                                                         (hx/fnc []
                                                           (let [[_ set-state] (hooks/use-state 0)]
                                                             (mxr/set-state-record me set-state)
                                                             (apply $ rn/View
                                                               {:style #js {:flex 1, :alignItems "center", :justifyContent "center"}}
                                                               {}
                                                               (doall (map #(mget % :rendering)
                                                                        (mget me :kids))))))))}
                                       {}
                                       (cFkids
                                         (mxr/make-rnc-ex "text"
                                           {:name      :counter42
                                            :content   "baby steps" ;; (cF (str "Boom " (mget me :counter)))
                                            :title     (cF (str "Better " (mget me :counter)))
                                            :rendering (cF ($ (hx/fnc []
                                                                (let [[_ set-state] (hooks/use-state 0)]
                                                                  (mxr/set-state-record me set-state)
                                                                  (prn "prn")
                                                                  (println "println")

                                                                  (apply $ rn/Button
                                                                    {:style   #js {:fontSize 36}
                                                                     :title   (mget me :title)
                                                                     :onPress ;; #(prn :press %)
                                                                     #(do ;; let [ctr (mxr/mx* me :counter42)]
                                                                          (prn :pressed! (mget me :counter))
                                                                          (mswap! me :counter inc))}
                                                                    {})))))}
                                           {:counter (cI 42)})
                                         #_(mxr/make-rnc-ex "button"
                                             {:name      :counter42
                                              :content   (cF (str "Boom " (mget me :counter)))
                                              :rendering (cF ($ Title3 {:me       me :sid (mget me :sid) :mas (mget me :hiya)
                                                                        :on-click #(let [ctr (mxr/mx* me :counter42)]
                                                                                     (mswap! me :counter inc))}))}
                                             {:counter (cI 42)})
                                         (mxr/make-rnc-ex "text"
                                           {:name      :counter42
                                            :content   "baby steps" ;; (cF (str "Boom " (mget me :counter)))
                                            :rendering (cF ($ (hx/fnc []
                                                                (let [[_ set-state] (hooks/use-state 0)]
                                                                  (mxr/set-state-record me set-state)
                                                                  (apply $ rn/Text
                                                                    {:style #js {:fontSize 18}} {} "baby steps rizing")))))})
                                         #_(mxr/make-rnc-ex "button"
                                             {:name      :counter42
                                              :content   (cF (str "Boom " (mget me :counter)))
                                              :rendering (cF ($ (hx/fnc []
                                                                  (let [[_ set-state] (hooks/use-state 0)]
                                                                    (mxr/set-state-record me set-state)
                                                                    (apply $ rn/Button
                                                                      {:title               (mget me :content)
                                                                       :color               "#841584"
                                                                       :accessibility-label "Increment this purple counter"
                                                                       :on-press            #(mswap! me :counter inc)}
                                                                      {})))))}
                                             {:counter (cI 42)}))))))))

(comment
  (mxr/make-rx
    :feed (cF (str "Fed Content " (mget (mxr/mxu! me :counter42) :counter)))
    :rendering (cFonce ($ (lambdac [{:keys [me]}]
                            (let [[_ set-state] (hooks/use-state 0)]
                              (mxr/set-state-record me set-state) ;; <-- used by Matrix on-change handler to trigger re-render
                              (d/h1 {} {}
                                (str "div6 " (mget me :feed))))) {:me me}))))
#_(defn mx-find-matrix [mx]
    (mxu-find-type mx ::hxApp))
