(ns tiltontec.util.core
  #?(:cljs (:require-macros
            [tiltontec.util.core :refer [prog1]]
            [tiltontec.util.ref :refer [ref-swap! make-ref]]))
  (:require
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])
   #?(:clj [tiltontec.util.ref :refer [make-ref ref-swap!]])))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn mx-type [it]
  (or (when-let [m (meta it)]
        (:mx-type m))
      (type it)))

(defn mx-type? [it type]
  (isa? (mx-type it) type))

(defn set-ify [x]
  (cond
    (nil? x) #{}
    (set? x) x
    (sequential? x) (set x)
    :else #{x}))

(defmacro prog1 [& body]
  `(let [result# ~(first body)]
     ~@(rest body)
     result#))

(defn pr-code-str [code]
  (with-out-str (binding [*print-level* 20]
                  (pprint code))))

;; --- error handling -----------------

(defmacro throw-ex
  ([msg]
   `(throw (ex-info ~msg {})))
  ([msg map]
   `(throw (ex-info ~msg ~map)))
  ([msg map cause]
   `(throw (ex-info ~msg ~map ~cause))))

;;; --- FIFO Queue -----------------------------

(defn make-fifo-queue [] (make-ref []))
(defn fifo-data [q] @q)
(defn fifo-clear [q] (ref-swap! q empty))
(defn fifo-empty? [q] (empty? @q))
(defn fifo-peek [q] (first @q))
(defn fifo-add [q new] (ref-swap! q conj new))
(defn fifo-pop [q]
  (when-not (fifo-empty? q)
    (prog1 (first @q)
           (ref-swap! q subvec 1))))
