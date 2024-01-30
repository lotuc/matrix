(ns hooks.mxreact.mxreact)

(defn mk* [sth jsx-props kids]
  (let [[mx-props kids] (if (map? (first kids))
                          [(first kids) (rest kids)]
                          [{} kids])]
    `(let [~'me nil
           ~'_cell nil
           ~'_prop-name nil
           ~'_cache nil]
       [~'me ~'_cell ~'_prop-name ~'_cache]
       (do ~sth
           (cljs.core/clj->js ~jsx-props)
           (doseq [k# (apply flatten [~@kids])]
            ;; tell the analyzer we're actually using the value
             (pr k#))
           ~@(map second mx-props)))))

(defmacro mk [sth mx-props & kids]
  (mk* sth mx-props kids))

(defmacro tag [mx-props & kids]
  (mk* nil mx-props kids))
