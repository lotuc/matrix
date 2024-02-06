(ns tiltontec.model.kids-test
  {:clj-kondo/ignore [:redundant-do]}
  (:require
   #?(:clj  [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [deftest is]])
   #?(:cljs [tiltontec.cell.core :refer-macros [cF with-mx] :refer [cI]]
      :clj  [tiltontec.cell.core :refer [cF cI with-mx]])
   #?(:clj  [tiltontec.model.family :refer [the-kids]]
      :cljs [tiltontec.model.family :refer-macros [the-kids]])
   #?(:clj  [tiltontec.model.navigate :refer [mdv!]]
      :cljs [tiltontec.model.navigate :refer-macros [mdv!]])
   [tiltontec.cell.base :refer [md-ref?] :as cty]
   [tiltontec.model.accessors :refer [mget mset!]]
   [tiltontec.model.core :refer [make]]
   [tiltontec.util.core :refer [mx-type?]]))

(deftest k-notq2be
  (with-mx
    (let [f (make :tiltontec.model/family
                  :ee (cI 2)
                  :kids (cF (the-kids
                             (when (odd? (mget me :ee))
                               (make
                                :name :yep
                                :value (cF (do
                                             (let [par (:parent @me)]
                                               (let [ee (mget par :ee)]
                                                 (* 14 ee))))))))))]
      (is (mx-type? f :tiltontec.model/family))
      (is (empty? (mget f :kids)))

      (do
        (mset! f :ee 3)
        (is (seq (mget f :kids)))
        (is (= 42 (mdv! :yep :value f)))

        (let [dmw (first (mget f :kids))]
          (assert (md-ref? dmw))
          (mset! f :ee 0)
          (is (empty? (mget f :kids)))
          (is (nil? @dmw))
          (is (= :dead (::cty/state (meta dmw)))))))))

#?(:cljs (do
           (cljs.test/run-tests)))
