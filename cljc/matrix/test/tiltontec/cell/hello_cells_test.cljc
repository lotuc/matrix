(ns tiltontec.cell.hello-cells-test
  {:clj-kondo/ignore [:unused-binding]}
  (:require
   #?(:clj [clojure.test :refer :all]
      :cljs [cljs.test :refer-macros [deftest is use-fixtures]])
   #?(:cljs [tiltontec.util.trace :refer-macros [trx]]
      :clj  [tiltontec.util.trace :refer [trx]])
   #?(:clj [tiltontec.cell.base :refer [cells-init] :as cty]
      :cljs [tiltontec.cell.base :refer [cells-init] :as cty])
   #?(:cljs [tiltontec.cell.core :refer-macros [cF+]
             :refer [c-reset! cI make-c-formula make-cell]]
      :clj [tiltontec.cell.core :refer [c-reset! cF+ cI make-c-formula make-cell]])
   [tiltontec.cell.diagnostic :refer [*mx-trace*]]
   [tiltontec.cell.evaluate :refer [cget]]
   [tiltontec.cell.poly :refer [c-awaken]]
   [tiltontec.matrix.api :refer [fn-watch]]))

(defn prn-level-3 [f]
  (binding [*print-level* 3] (f)))

(use-fixtures :once prn-level-3)

(deftest hw-01
  (cells-init)
  (let [v ;;"visitor"
        {:name "World"
         :action (make-cell :value "knocks"
                            :input? true)}]

    (println (cget (:name v))
             (cget (:action v)))

    (is (=  (cget (:name v)) "World"))
    (is (=  (cget (:action v)) "knocks"))))

(deftest hw-02
  (cells-init)
  (let [watch-action (atom nil)
        v ;;"visitor"
        {:name "World"
         :action (cI nil
                     :prop :v-action
                     :watch ;; short for watch
                     (fn [prop me new old c]
                       (reset! watch-action new)
                       (println :watcherving prop new old)))}]
    (is (=  (cget (:name v)) "World"))
    (c-reset! (:action v) "knocks")
    (is (=  (cget (:action v)) "knocks"))
    (is (= "knocks" @watch-action))))

(deftest hw-03
  (cells-init)
  (let [action (atom nil)
        watch-action (fn [prop me new old c]
                       (reset! action new)
                       (println :watcherving prop new old))
        v {:name "World"
           :action (cI nil :prop :v-action
                       :watch watch-action)}]

    (is (nil? (cget (:action v))))
    (is (nil? @action))

    (c-reset! (:action v) "knock-knock")
    (is (= "knock-knock" @action))
    (is (= (cget (:action v)) "knock-knock"))))

(defn gwatch
  [prop me new old c]
  (println :gwatch> prop new old))

(deftest hw-04
  (cells-init)
  (let [r-action (cI nil
                     :prop :r-action
                     :watch gwatch)
        r-loc (make-c-formula
               :prop :r-loc
               :watch gwatch
               :rule (fn [c]
                       (case (cget r-action)
                         :leave :away
                         :return :at-home
                         :missing)))]
    (c-awaken r-loc)
    (is (= :missing (:value @r-loc)))
    (println :---about-to-leave------------------)
    (c-reset! r-action :leave)
    (println :---left------------------)
    (is (= :away (cget r-loc)))))

(deftest hw-5
  (cells-init)
  (println :--go------------------)
  (let [watch-action (fn [prop me new old c]
                       (println prop new old))
        v {:name "World"
           :action (cI nil :prop :v-action
                       :watch watch-action)}
        r-action (cI nil)
        r-loc (cF+ [:watch (fn-watch (when new (trx :honey-im new)))]
                   (case (cget r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (cF+ [:watch (fn-watch (trx :r-resp new))]
                        (when (= :home (cget r-loc))
                          (when-let [act (cget (:action v))]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (cget r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (cget r-loc)))))

(deftest hello-world
  (cells-init)
  (println :--go------------------)
  (let [watch-action (fn [prop me new old c]
                       (println prop new old))
        v {:name "World"
           :action (cI nil
                       :prop :v-action
                       :ephemeral? true
                       :watch watch-action)}
        r-action (cI nil)
        r-loc (cF+ [:watch (fn-watch (when new (trx :honey-im new)))]
                   (case (cget r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (cF+ [:watch (fn-watch (trx :r-response new))
                         :ephemeral? true]
                        (when (= :home (cget r-loc))
                          (when-let [act (cget (:action v))]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (cget r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (cget r-loc)))
    (c-reset! (:action v) :knock-knock)))

(deftest hello-world-2
  (cells-init)
  (println :--go------------------)
  (let [watch-action (fn [prop me new old c]
                       (when new (trx visitor-did new)))
        v {:name "World"
           :action (cI nil
                       :prop :v-action
                       :ephemeral? true
                       :watch watch-action)}
        r-action (cI nil)
        r-loc (cF+ [:watch (fn-watch (when new (trx :honey-im new)))]
                   (case (cget r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (cF+ [:watch (fn-watch (when new
                                            (trx :r-response new)))
                         :ephemeral? true]
                        (when (= :home (cget r-loc))
                          (when-let [act (cget (:action v))]
                            (case act
                              :knock-knock "hello, world"))))
        alarm (cF+ [:watch (fn-watch
                            (trx :telling-alarm-api new))]
                   (if (= :home (cget r-loc)) :off :on))
        alarm-do (cF+ [:watch (fn-watch
                               (case new
                                 :call-police (trx :auto-dialing-911)
                                 nil))]
                      (when (= :on (cget alarm))
                        (when-let [action (cget (:action v))]
                          (case action
                            :smashing-window :call-police
                            nil))))]
    (c-awaken [alarm-do r-response r-loc (:action v)])
    (is (= :missing (:value @r-loc)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! (:action v) :smashing-window)
    (c-reset! r-action :return)
    (is (= :home (cget r-loc)))
    (c-reset! (:action v) :knock-knock)))

(comment
  ;; test tracing messages
  (binding [*mx-trace* :all]
    (let [x (cI 1 :prop :x :debug true)
          b (cF+ [:prop :b] (inc (cget x)))
          a (cF+ [:prop :a] (* (cget x) (cget b)))
          c (cF+ [:prop :c :optimize :when-value-t]
                 (let [v (cget x)]
                   (when (> v 2)
                     v)))
          h (cF+ [:prop :h] (when (> (cget b) 4)
                              (cget b)
                              (* (cget a) 2)))
          g (cF+ [:prop :g :lazy true] (when (> (cget b) 4)
                                         (cget b)
                                         (* (cget a) 2)))]
      (cget h)
      (println "^^^^^^^^^^^^^^^^^^ (cget h)")
      (cget a)
      (println "^^^^^^^^^^^^^^^^^^ (cget a)")
      (cget c)
      (println "^^^^^^^^^^^^^^^^^^ (cget c)")
      (cget g)
      (println "^^^^^^^^^^^^^^^^^^ (cget g)")
      (c-reset! x 3)
      (println "^^^^^^^^^^^^^^^^^^ (c-reset! x 3)")
      (cget c)
      (println "^^^^^^^^^^^^^^^^^^ (cget c)")
      (cget g)
      (println "^^^^^^^^^^^^^^^^^^ (cget g)"))))

#?(:cljs (do (cljs.test/run-tests)))
