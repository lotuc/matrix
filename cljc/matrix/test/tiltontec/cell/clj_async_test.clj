(ns tiltontec.cell.clj-async-test
  (:require
   [clojure.core.async :as a]
   [clojure.test :refer :all]
   [tiltontec.cell.core :refer [cF+ cget cI cset! with-mx]]))

(deftest test-async
  (doseq [typ [:future :promise :chan]]
    (testing (str "value type: " typ)
      (with-mx
        (let [ch (a/chan 1)
              a (cI 1)
              b (cF+ [:async? true]
                     (let [a-val (cget a)
                           r (case typ
                               :future (future (a/<!! ch) (/ 42 a-val))
                               :promise (promise)
                               :chan (a/chan))]
                       (when-not (= typ :future)
                         (a/go
                           (a/<! ch)
                           (let [v (try (/ 42 a-val) (catch Exception _ nil))]
                             (case typ
                               :promise (deliver r v)
                               :chan (do (when v (a/put! r v))
                                         (a/close! r))))))
                       r))

              fire-and-wait!
              #(do (a/put! ch :go)
                   (Thread/sleep 20))]
          (is (= 1 (cget a)))

          (is (nil? (cget b)))
          (fire-and-wait!)
          (is (= 42 (cget b)))

          (cset! a 2)
          (is (nil? (cget b)))
          (fire-and-wait!)
          (is (= 21 (cget b)))

         ;; default async value extraction for Clojure future ignores exceptions
          (cset! a 0)
          (is (nil? (cget b)))
          (fire-and-wait!)
          (is (nil? (cget b))))))))

(deftest test-async-keep-last
  (with-mx
    (let [ch (a/chan 1)
          a (cI 1)
          b (cF+ [:async? {:keep-last? true}]
                 (let [a-val (cget a)]
                   (future (a/<!! ch) (/ 42 a-val))))
          fire-and-wait!
          #(do (a/put! ch :go)
               (Thread/sleep 20))]
      (is (= 1 (cget a)))

      (is (nil? (cget b)))
      (fire-and-wait!)
      (is (= 42 (cget b)))

      (cset! a 2)
      (is (= 42 (cget b)) "should keep last value")
      (fire-and-wait!)
      (is (= 21 (cget b))))))

(deftest test-async-with-async-then
  (let [ch (a/chan 1)
        a (cI 1)
        b (cF+ [:async? {:then (fn [ok err]
                                 (if err [:err err] [:ok ok]))}]
               (let [a-val (cget a)]
                 (future (a/<!! ch) (/ 42 a-val))))

        fire-and-wait! #(do (a/put! ch :go)
                            (Thread/sleep 20))]
    (is (= 1 (cget a)))

    (is (nil? (cget b)))
    (fire-and-wait!)
    (is (= [:ok 42] (cget b)))

    (cset! a 2)
    (is (nil? (cget b)))
    (fire-and-wait!)
    (is (= [:ok 21] (cget b)))

    (cset! a 0)
    (is (nil? (cget b)))
    (fire-and-wait!)
    (is (= :err (first (cget b))))))

(comment
  ;; playing with additional async options
  (do (def a (cI 1))
      (def b (cF+ [:async? {:keep-last? true
                            ;; :abort-last? true
                            ;; :pending-value :pending
                            ;; :then (fn [ok err] [ok err])
                            }]
                  (let [a-val (cget a)]
                    (future (Thread/sleep (long (+ 500 (rand-int 500)))) (/ 42 a-val)))))
      (def c (cF+ [:watch (fn [_ _ new old _]
                            (println ":c" old "->" new))]
                  (cget b)))

      (cget c)
      (cset! a 2)))
