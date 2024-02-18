(ns tiltontec.cell.async
  #?(:cljs (:require-macros [tiltontec.cell.async]))
  (:require
   #?(:clj  [tiltontec.cell.base :refer [c-warn]]
      :cljs [tiltontec.cell.base :refer-macros [c-warn]])
   #?(:clj  [clojure.core.async :refer [<! go]]
      :cljs [cljs.core.async :refer [<! go]])
   #?(:cljs [cljs.core.async.impl.channels :refer [ManyToManyChannel]]))
  #?(:clj (:import
           [clojure.core.async.impl.channels ManyToManyChannel])))

(defprotocol AsyncValue
  "A protocol for async values."
  (then [this f]))

#?(:cljs
   (extend-protocol AsyncValue
     js/Promise
     (then [this f]
       (.then this f
              #(c-warn "AsyncValue then caught unhandled exception: " %)))))

#?(:clj
   (extend-protocol AsyncValue
     clojure.lang.IDeref
     (then [this f]
       (try (f @this)
            (catch Throwable t
              (c-warn "AsyncValue then caught unhandled exception: " t))))))

(extend-protocol AsyncValue
  ManyToManyChannel
  (then [ch f]
    (go (f (<! ch)))))

(defmacro async-run! [& body]
  (if (:ns &env)
    `(js/Promise. (fn [resolve# _#]
                    (try ~@body (finally (resolve# nil)))))
    `(future ~@body)))
