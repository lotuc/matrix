(ns tiltontec.model.accessors
  (:require
   [tiltontec.cell.base :refer [c-warn md-ref?]]
   [tiltontec.cell.core :refer [cset!]]
   [tiltontec.cell.evaluate :refer [cget]]
   [tiltontec.util.core :refer [mx-type throw-ex]]))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn- md-cell [me prop]
  (prop (:cz (meta me))))

(def ^:private no-such-prop (gensym))

(defn mget?
  ([me prop]
   (mget? me prop nil))
  ([me prop alt-val]
   (assert me (str "mget passed nil for me accessing prop: " prop))
   (assert (md-ref? me) (str "mget passed non-model for me accessing prop: " prop ": " me))
   (if (contains? @me prop)
     (if-let [c (md-cell me prop)]
       (cget c)
       (prop @me))
     alt-val)))

(defn mget [me prop]
  (let [v (mget? me prop no-such-prop)]
    (when (= v no-such-prop)
      (c-warn
       "MXAPI_ILLEGAL_GET_NO_SUCH_PROP> mget was attempted on non-existent prop \"" prop "\"."
       "\n...> FYI: known props are: " (keys @me)
       "\n...> FYI: use mget? if prop might not exist.")
      (throw-ex "MXAPI_ILLEGAL_GET_NO_SUCH_PROP> mget was attempted on non-existent prop" {:model me :prop prop}))
    v))

(defn mset! [me prop new-value]
  (assert me (str "mset! passed nil for me accessing prop: " prop))
  (assert (md-ref? me) (str "mset! passed non-model for me setting prop: " prop ": " me))

  (if-let [c (md-cell me prop)]
    (cset! c new-value)
    (throw-ex
     (if (contains? @me prop)
       (do (c-warn
            "MXAPI_ILLEGAL_MUTATE_NONCELL> invalid mswap!/mset!/mset! to the property '" prop "', which is not mediated by any cell.\n"
            "...> if such post-make mutation is in fact required, wrap the initial argument to model.core/make in 'cI'. eg: (make... :answer (cI 42)).\n"
            "...> look for MXAPI_ILLEGAL_MUTATE_NONCELL in the Errors documentation for  more details.\n"
            "...> FYI: intended new value is [" new-value "]; initial value was [" (get @me prop :no-such-prop) "].\n"
            "...> FYI: instance is of type " (mx-type me) ".\n"
            "...> FYI: full instance is " @me "\n"
            "...> FYI: instance meta is " (meta me) "\n.")
           "MXAPI_ILLEGAL_MUTATE_NONCELL> invalid mswap!/mset!/mset! to the property which is not mediated by any cell")
       (do (c-warn "MXAPI_ILLEGAL_MUTATE_NO_SUCH_prop> mswap!/mset!/mset! was attempted to non-existent prop \"" prop "\".\n"
                   "...> FYI: known props are: " (keys @me))
           "MXAPI_ILLEGAL_MUTATE_NO_SUCH_prop> mswap!/mset!/mset! was attempted to non-existent prop"))
     {:model me :prop :prop :new-value new-value})))

(defn mreset!
  "alternate syntax conforming with clojure terminology"
  [me prop new-value]
  (mset! me prop new-value))

(defn mswap! [me prop swap-fn & swap-fn-args]
  (mset! me prop (apply swap-fn (mget me prop) swap-fn-args)))

(defmacro def-mget [reader-prefix & props]
  `(do ~@(map (fn [prop#]
                `(defn ~(symbol (str (or reader-prefix "") (name prop#)))
                   [~'ref]
                   (tiltontec.model.accessors/mget ~'ref ~(keyword (name prop#)))))
              props)))
