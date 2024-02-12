(ns tiltontec.model.opti-test
  (:require
   #?(:clj  [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [deftest is]])
   #?(:clj  [tiltontec.util.ref :refer [dosync!]]
      :cljs [tiltontec.util.ref :refer [dosync!]])
   [tiltontec.cell.base :as cty]
   [tiltontec.cell.core :refer [cI]]
   [tiltontec.cell.poly :refer [md-quiesce]]
   [tiltontec.matrix.api :refer [cF cF+ cf-freeze with-mx]]
   [tiltontec.model.accessors :refer [mget mset! mswap!]]
   [tiltontec.model.core :refer [make]]))

(deftest opti-map-value
  (with-mx
    (let [me (make
              :style (cF {:color :red}))]
      (is (= (mget me :style)
             {:color :red}))
      (is (= (mget me :style)
             {:color :red}))
      (prn :flushed (:cells-flushed (meta me)))
      (prn :cz (:cz (meta me)))
      (prn :style (:style @me)))))

(deftest md-freeze-default
  (with-mx
    (let [fm (make
              :aa (cI 1)
              :bb (cF (cond
                        (= 2 (mget me :aa)) (cf-freeze)
                        (> (mget me :aa) 2) 43
                        :else 42)))]
      (is (= 1 (mget fm :aa)))
      (is (= 42 (mget fm :bb)))
      (mswap! fm :aa inc)
      (is (= 2 (mget fm :aa)))
      (is (= 42 (mget fm :bb)))

      (mswap! fm :aa inc)
      (is (= 3 (mget fm :aa)))
      (is (= 42 (mget fm :bb))))))

(deftest md-freeze-specific
  (with-mx
    (let [fm (make
              :aa (cI 1)
              :bb (cF (cond
                        (= 2 (mget me :aa)) (cf-freeze 17)
                        (> (mget me :aa) 2) 43
                        :else 42)))]
      (is (= 1 (mget fm :aa)))
      (is (= 42 (mget fm :bb)))
      (mswap! fm :aa inc)
      (is (= 2 (mget fm :aa)))
      (is (= 17 (mget fm :bb)))

      (mswap! fm :aa inc)
      (is (= 3 (mget fm :aa)))
      (is (= 17 (mget fm :bb))))))

(deftest md-freeze-model-&-cell-lifecycle-functions
  (with-mx
    (let [c1-quiesce-record (atom nil)
          c1-watch-record (atom [])
          fm-quiesce-record (atom nil)
          fm (make
              :on-quiesce (fn [md] (reset! fm-quiesce-record @md))
              :c0 (cI 0)
              :c1 (cF+ [:on-quiesce (fn [c] (reset! c1-quiesce-record @c))
                        :watch (fn [_prop me new old _c]
                                 (swap! c1-watch-record conj [(mget me :c0) old new]))]
                       (if (= (mget me :c0) -1)
                         (cf-freeze)
                         (mget me :c0))))]

      (is (nil? @c1-quiesce-record))

      (is (= 0 (mget fm :c1)))
      (is (= [[0 cty/unbound 0]] @c1-watch-record))

      (mset! fm :c0 42)
      (is (= [[0 cty/unbound 0] [42 0 42]] @c1-watch-record))
      (is (= 42 (mget fm :c1)))

      (mset! fm :c0 -1)
      ;; c1's value is not changed, but c1 is now optimized away because of c0
      ;; becomes -1, so the watch function is called for the last time.
      (is (= [[0 cty/unbound 0] [42 0 42] [-1 42 42]] @c1-watch-record))
      (is (= 42 (mget fm :c1)))

      ;; c1 is optimized away, that cause on-quiesce function to be *never*
      ;; called!.
      (dosync! (md-quiesce fm))
      (is (nil? @c1-quiesce-record))

      ;; the model's on-quiesce function is called on `md-quiesce`
      (is (some? @fm-quiesce-record)))))
