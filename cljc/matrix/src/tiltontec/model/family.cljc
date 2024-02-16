(ns tiltontec.model.family
  #?(:cljs (:require-macros
            [tiltontec.model.family :refer [cFkids]]))
  (:require
   #?(:cljs [tiltontec.cell.core :refer-macros [cF]]
      :clj  [tiltontec.cell.core :refer [cF]])
   [clojure.set :refer [difference]]
   [tiltontec.cell.base :refer [md-ref? unbound] :as cty]
   [tiltontec.cell.diagnostic :refer [minfo mxtrc]]
   [tiltontec.cell.poly :refer [md-quiesce md-quiesce-self watch]]
   [tiltontec.model.accessors :refer [mget]]
   [tiltontec.model.core :refer [*parent*]]))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn md-kids [me] (mget me :kids))

(defn fm-kids-watch [me newk oldk _c]
  (when-not (= oldk unbound)
    (let [lostks (difference (set oldk) (set newk))]
      (when-not (empty? lostks)
        (mxtrc :quiesce :fm-kids-watch (minfo me) :lostks (count lostks))
        (doseq [k lostks :when (md-ref? k)]
          (md-quiesce k))))))

(defmethod watch [:kids :tiltontec.model/family]
  [_prop me newk oldk c]
  (fm-kids-watch me newk oldk c))

(defmethod md-quiesce :tiltontec.model/family
  [me]
  (mxtrc :quiesce :family-md-quies-entry! (minfo me))
  (doseq [k (:kids @me) :when (md-ref? k)]
    (mxtrc :quiesce :family-md-quiKID! (minfo me))
    (md-quiesce k))
  (md-quiesce-self me))

(defmacro the-kids
  "Macro to flatten kids in 'tree' and relate them to 'me' via the *parent* dynamic binding"
  [& tree]
  `(binding [tiltontec.model.core/*parent* ~'me]
     (assert tiltontec.model.core/*parent*)
     (doall (remove nil? (flatten (list ~@tree))))))

(defmacro cFkids
  "Syntax sugar for formulae that define :kids props"
  [& tree]
  `(cF (assert ~'me "no me for cFkids")
       (the-kids ~@tree)))

(defn kid-values-kids
  "A pattern commonly employed in matrix applications is to define a :kid-factory on some
   'parent' cell, and use it to enrich the value extracted from the parent's kid cells.

   This function maps across the :kids-values, invoking the factory as it goes"
  [me x-kids]
  (let [k-key (mget me :kid-key)
        _ (assert k-key ":kid-key not found")
        k-factory (mget me :kid-factory)
        _ (assert k-factory ":kid-factory not found.")

        x-kids (when (not= x-kids unbound)
                 (into {} (for [k x-kids]
                            [(k-key k) k])))]

    (doall
     (map
      (fn [kid-value]
        (or (and x-kids (get x-kids kid-value))
            (binding [*parent* me]
              (k-factory me kid-value))))
      (mget me :kid-values)))))
