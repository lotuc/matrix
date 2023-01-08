(ns tiltontec.model.opti-test
  (:require
    [clojure.string :as str]
    [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint cl-format] :as pp]
    #?(:clj [clojure.test :refer :all]
       :cljs [cljs.test
              :refer-macros [deftest is are]])
    #?(:cljs [tiltontec.util.base
              :refer-macros [trx prog1 *trx?*]]
       :clj  [tiltontec.util.base
              :refer :all])
    [tiltontec.util.core :refer [type-of err]]
    #?(:clj [tiltontec.cell.base :refer :all :as cty]
       :cljs [tiltontec.cell.base
              :refer-macros [without-c-dependency]
              :refer [cells-init c-optimized-away? c-formula? c-value c-optimize
                      c-unbound? c-input? ia-type?
                      c-model mdead? c-valid? c-useds c-ref? md-ref?
                      c-state +pulse+ c-pulse-observed
                      *call-stack* *defer-changes* unbound
                      c-rule c-me c-value-state c-callers caller-ensure
                      unlink-from-callers *causation*
                      c-slot-name c-synaptic? caller-drop
                      c-pulse c-pulse-last-changed c-ephemeral? c-slot c-slots
                      *depender* *not-to-be*
                      *c-prop-depth* md-slot-owning? c-lazy] :as cty])
    #?(:cljs [tiltontec.cell.integrity
              :refer-macros [with-integrity with-cc]]
       :clj [tiltontec.cell.integrity :refer [with-integrity with-cc]])
    #?(:clj [tiltontec.cell.observer
             :refer [defobserver fn-obs]]
       :cljs [tiltontec.cell.observer
              :refer-macros [defobserver fn-obs]])

    #?(:cljs [tiltontec.cell.core
              :refer-macros [cF cF+ c-reset-next! cFonce cFn]
              :refer [cI c-reset! make-cell make-c-formula]]
       :clj [tiltontec.cell.core :refer :all])

    [tiltontec.cell.evaluate :refer [c-get c-awaken]]
    [tiltontec.model.base :refer [md-cz md-cell]]
    #?(:clj [tiltontec.model.core :refer :all :as md]
       :cljs [tiltontec.model.core
              :refer-macros [cFkids the-kids mdv!]
              :refer [md-get md-name fget fm! make md-reset! md-getx]
              :as md])
    ))

; (deftest t-freeze-default
;  ; confirm (cf-freeze) behaves same as (cf-freeze _cache)
;  (cells-init)
;
;  (let [a (cI 1 :slot :aa)
;        b (cF+ [:slot :bb]
;            (if (= 2 (c-get a))
;              (cf-freeze)
;              42))]
;    (is (= 1 (c-get a)))
;    (is (= 42 (c-get b)))
;    (c-swap! a inc)
;    (is (= 2 (c-get a)))
;    (is (= 42 (c-get b)))
;    (is (c-optimized-away? b))
;    (is (not (seq (c-callers a))))))

(deftest md-freeze-default
  (cells-init)

  (let [fm (md/make
             :aa (cI 1)
             :bb (cF (cond
                       (= 2 (mget me :aa)) (cf-freeze)
                       (> (mget me :aa) 2) 43
                       :default 42)))]
    (is (= 1 (mget fm :aa)))
    (is (= 42 (mget fm :bb)))
    (mswap! fm :aa inc)
    (is (= 2 (mget fm :aa)))
    (is (= 42 (mget fm :bb)))

    (mswap! fm :aa inc)
    (is (= 3 (mget fm :aa)))
    (is (= 42 (mget fm :bb)))))

(deftest md-freeze-specific
  (cells-init)

  (let [fm (md/make
             :aa (cI 1)
             :bb (cF (cond
                       (= 2 (mget me :aa)) (cf-freeze 17)
                       (> (mget me :aa) 2) 43
                       :default 42)))]
    (is (= 1 (mget fm :aa)))
    (is (= 42 (mget fm :bb)))
    (mswap! fm :aa inc)
    (is (= 2 (mget fm :aa)))
    (is (= 17 (mget fm :bb)))

    (mswap! fm :aa inc)
    (is (= 3 (mget fm :aa)))
    (is (= 17 (mget fm :bb)))))