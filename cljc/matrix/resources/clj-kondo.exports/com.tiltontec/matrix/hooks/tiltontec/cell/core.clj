(ns hooks.tiltontec.cell.core)

(defmacro cF [& body]
  `(let [~'me nil
         ~'_cell nil
         ~'_prop-name nil
         ~'_cache nil]
     [~'me ~'_cell ~'_prop-name ~'_cache]
     ~@body))

(defmacro cF+ [[& options] & body]
  `(let [~'me nil
         ~'_cell nil
         ~'_prop-name nil
         ~'_cache nil]
     (apply hash-map [~@options])
     [~'me ~'_cell ~'_prop-name ~'_cache]
     ~@body))
