(ns tiltontec.util.ref)

(set! *warn-on-reflection* true)

;; --- refs with maps conveniences -------------------

(defmacro dosync! [& body]
  (if (:ns &env)
    `(do ~@body)
    `(dosync ~@body)))

(defmacro make-ref
  ([x]
   (if (:ns &env)
     `(atom ~x)
     `(ref ~x)))
  ([x & options]
   (if (:ns &env)
     `(atom ~x ~@options)
     `(ref ~x ~@options))))

(defmacro any-ref? [x]
  (if (:ns &env)
    `(instance? cljs.core.Atom ~x)
    `(instance? clojure.lang.Ref ~x)))

(defmacro ref-swap! [ref f & args]
  (if (:ns &env)
    `(swap! ~ref ~f ~@args)
    `(alter ~ref ~f ~@args)))

(defmacro ref-set! [ref v]
  (if (:ns &env)
    `(reset! ~ref ~v)
    `(ref-set ~ref ~v)))

(defmacro rmap-swap-prop!
  "Swap the value of a property in a ref map, returning the new value."
  [ref prop f & args]
  `(let [p# ~prop]
     (-> (ref-swap! ~ref update p# ~f ~@args)
         (get p#))))

(defmacro rmap-set-prop!
  "Set the value of a property in a ref map, returning the new value."
  [ref prop new-value]
  `(let [v# ~new-value]
     (do (ref-swap! ~ref assoc ~prop v#) v#)))

(defmacro meta-map-swap-prop!
  "Swap the value of a property in a ref map's meta, returning the new value."
  [x meta-prop f & args]
  `(let [p# ~meta-prop]
     (-> (alter-meta! ~x update p# ~f ~@args)
         (get p#))))

(defmacro meta-map-set-prop!
  "Set the value of a property in a ref map's meta, returning the new value."
  [x meta-prop new-value]
  `(let [v# ~new-value]
     (do (alter-meta! ~x assoc ~meta-prop v#) v#)))

;; --- easy access to maps in refs ----

(defmacro def-rmap-props [reader-prefix & props]
  `(do ~@(map (fn [prop#]
                `(defn ~(symbol (str (or reader-prefix "") prop#))
                   [~'ref]
                   (~(keyword prop#) @~'ref))) props)))

(defmacro def-rmap-meta-props [reader-prefix & props]
  `(do ~@(map (fn [prop#]
                `(defn ~(symbol (str (or reader-prefix "") prop#))
                   [~'ref]
                   (~(keyword prop#) (meta ~'ref)))) props)))
