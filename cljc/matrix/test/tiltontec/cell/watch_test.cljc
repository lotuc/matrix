(ns tiltontec.cell.watch-test
  #?(:cljs (:require-macros
            [tiltontec.util.ref :refer [dosync!]]))
  (:require
   #?(:clj  [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [deftest is]])
   #?(:clj  [tiltontec.util.ref :refer [dosync!]])
   #?(:cljs [tiltontec.util.trace :refer-macros [trx]]
      :clj  [tiltontec.util.trace :refer [trx]])
   #?(:clj  [tiltontec.cell.base
             :refer [c-callers c-input? c-model c-prop c-prop-name c-useds
                     c-valid? c-value-state] :as cty]
      :cljs [tiltontec.cell.base
             :refer [c-callers c-input? c-model c-prop c-prop-name c-useds
                     c-valid? c-value-state] :as cty])
   #?(:cljs [tiltontec.cell.core :refer-macros [cF+ with-mx] :refer [c-reset! cI]]
      :clj  [tiltontec.cell.core :refer [c-reset! cF+ cI with-mx]])
   [tiltontec.cell.evaluate :refer [cget]]
   [tiltontec.matrix.api :refer [fn-watch]]
   [tiltontec.util.core :refer [mx-type?]]))

#?(:cljs (set! *print-level* 3))

(deftest t-formula
  (with-mx
    (let [bingo (atom false)
          c (cF+ [:prop :bingo
                  :watch (fn-watch
                          (reset! bingo true))]
                 (+ 40 2))]
      (is (mx-type? c ::cty/cell))
      (is (mx-type? c ::cty/c-formula))
      (is (= (c-value-state c) :unbound))
      (is (= #{} (c-callers c)))
      (is (= #{} (c-useds c)))
      (is (not (c-input? c)))
      (is (not (c-valid? c)))
      (is (nil? (c-model c)))
      (is (= (cget c) 42))
      (is (= 42 @c))                                        ;; ie, optimized-away
      (is @bingo))))

(def bingo2 (atom false))

(deftest test-input
  (with-mx
    (let [c (cI 42 :prop :bingo2
                :watch (fn-watch (reset! bingo2 true)))]
      (is (mx-type? c ::cty/cell))
      (is (= (c-value-state c) :valid))
      (is (= #{} (c-callers c)))
      (is (c-input? c))
      (is (c-valid? c))
      (is (nil? (c-model c)))
      (is (= :bingo2 (c-prop c) (c-prop-name c)))
      (is (= (cget c) 42))
      (is (= false @bingo2)))))

(deftest test-input-watch
  (with-mx
    (let [c (cI 42 :prop :bingo2
                :watch (fn-watch (reset! bingo2 true)))]
      (is (mx-type? c ::cty/cell))
      (is (= (c-value-state c) :valid))
      (is (= #{} (c-callers c)))
      (is (c-input? c))
      (is (c-valid? c))
      (is (nil? (c-model c)))
      (is (= :bingo2 (c-prop c) (c-prop-name c)))
      (is (= (cget c) 42))
      (is (= false @bingo2)))))

(deftest t-custom-watch
  (with-mx
    (let [bwatch (atom nil)
          b (cI 2 :prop :bb
                :watch (fn-watch
                        (trx nil prop me new old)
                        (reset! bwatch new)))
          cwatch (atom nil)
          c (cF+ [:watch (fn-watch [prop me new old c]
                                   (trx prop me new old)
                                   (reset! cwatch new))]
                 (* 10 (cget b)))]
      (dosync!
       (is (= (cget b) 2))
       (is (= @bwatch nil))
       (is (= (cget c) 20))
       (is (= @cwatch 20))
       (c-reset! b 3)
       (is (= 3 @bwatch))
       (is (= 30 (cget c)))
       (is (= 30 @cwatch))))))

#?(:cljs (cljs.test/run-tests))
