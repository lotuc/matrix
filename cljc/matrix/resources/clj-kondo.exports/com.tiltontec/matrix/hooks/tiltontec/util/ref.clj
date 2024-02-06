(ns hooks.tiltontec.util.ref)

(defmacro def-rmap-props [reader-prefix & props]
  `(do ~@(map (fn [prop#]
                `(defn ~(symbol (str (or reader-prefix "") prop#))
                   [~'ref]
                   (~(keyword prop#) @~'ref)))
              props)))

(defmacro def-rmap-meta-props [reader-prefix & props]
  `(do ~@(map (fn [prop#]
                `(defn ~(symbol (str (or reader-prefix "") prop#))
                   [~'ref]
                   (~(keyword prop#) (meta ~'ref))))
              props)))
