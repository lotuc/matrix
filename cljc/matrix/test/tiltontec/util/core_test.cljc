(ns tiltontec.util.core-test
  #?(:cljs (:require-macros
            [tiltontec.util.ref
             :refer [any-ref? dosync! make-ref make-ref rmap-set-prop!
                     rmap-swap-prop! def-rmap-props]]))
  (:require
   #?(:cljs [cljs.test :refer-macros [deftest is are use-fixtures]]
      :clj [clojure.test :refer :all])
   #?(:cljs [tiltontec.util.trace :refer-macros [trx wtrx]]
      :clj [tiltontec.util.trace :refer [*trxdepth* trx wtrx]])
   #?(:clj [tiltontec.util.ref
            :refer [any-ref? def-rmap-props dosync! make-ref make-ref
                    rmap-set-prop! rmap-swap-prop!]])
   #?(:clj [tiltontec.util.core
            :refer [fifo-add fifo-data fifo-empty? fifo-peek fifo-pop
                    make-fifo-queue prog1 set-ify throw-ex]]
      :cljs [tiltontec.util.core
             :refer [fifo-add fifo-data fifo-empty? fifo-peek fifo-pop
                     make-fifo-queue set-ify throw-ex]
             :refer-macros [prog1]])
   [clojure.set :as cset]))

(defn prn-level-3 [f]
  (binding [*print-level* 3] (f)))

(use-fixtures :once prn-level-3)

(deftest fake-cl
  (is (= 42 (prog1 42 43 44))))

(deftest setify
  (is (= #{1 2 3} (set-ify [1 1 2 2 3 3])))
  (is (= #{1 2 3} (set-ify (list 1 1 2 2 3 3))))
  (is (= #{} (set-ify nil)))
  (is (= #{42} (set-ify 42)))
  (is (= #{"bob"} (set-ify "bob")))
  (is (= #{{:a 13}} (set-ify {:a 13})))
  (is (= #{42}
         (cset/difference (set-ify [1 2 42])
                          (set-ify (list 1 2))))))

(def-rmap-props jj- boom)

(deftest test-rmap
  (let [x (make-ref {:value 0 :boom 42})]
    (is (= 42 (jj-boom x)))
    (is (= 0 (:value @x)))
    (dosync! (rmap-set-prop! x :value 42))
    (trx nil :xxx x @x (:value @x))
    (is (= 42 (:value @x)))
    (is (let [j (dosync! (rmap-set-prop! x :value 43))]
          (= 43 j)))
    (is (= 44 (dosync! (rmap-set-prop! x :value 44))))
    (is (= 45 (dosync! (rmap-swap-prop! x :value inc))))
    (is (= 50 (dosync! (rmap-swap-prop! x :value + 5))))))

(defn wtrx-test [n]
  (wtrx
   (0 10 "test" n)
   (when (> n 0)
     (wtrx-test (dec n)))))

(deftest err-handling
  (is (thrown? #?(:cljs js/Error :clj Exception)
               (throw-ex "boom")))
  (is (thrown-with-msg?
       #?(:cljs js/Error :clj Exception)
       #"oom"
       (throw-ex "boom")))

  (are [x] (not (any-ref? x))
    nil
    42
    [])

  #?(:clj
     (is (= "...... cool: 1, 2, 3\n:bingo\n"
            (with-out-str
              (binding [*trxdepth* 5]
                (wtrx (0 100 "cool" 1 2 3)
                      (println :bingo)))))))
  #?(:cljs
     (is (= ". test: 3\n.. test: 2\n... test: 1\n.... test: 0\n"
            (with-out-str
              (wtrx-test 3))))))

(deftest fifo-build
  (let [q (make-fifo-queue)]
    (is (fifo-empty? q))
    (is (nil? (fifo-peek q)))
    (is (nil? (fifo-pop q)))
    (is (empty? (fifo-data q)))
    (dosync!
     (fifo-add q 1)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (fifo-empty? q)))
    (dosync!
     (fifo-add q 1)
     (fifo-add q 2)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (= 2 (fifo-pop q)))
     (is (fifo-empty? q)))))

(deftest fifo-build-test
  (let [q (make-fifo-queue)]
    (is (fifo-empty? q))
    (is (nil? (fifo-peek q)))
    (is (nil? (fifo-pop q)))
    (is (empty? (fifo-data q)))
    (dosync!
     (fifo-add q 1)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (fifo-empty? q)))
    (dosync!
     (fifo-add q 1)
     (fifo-add q 2)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (= 2 (fifo-pop q)))
     (is (fifo-empty? q)))))

#?(:cljs (do (cljs.test/run-tests)))
