(ns tiltontec.cell.async
  #?(:cljs (:require-macros [tiltontec.cell.async]))
  (:require
   #?(:clj  [clojure.core.async :refer [<! go] :as a]
      :cljs [cljs.core.async :refer [<! go]])
   #?(:clj  [tiltontec.cell.base :refer [c-warn]]
      :cljs [tiltontec.cell.base :refer-macros [c-warn]])
   #?(:cljs [cljs.core.async.impl.channels :refer [ManyToManyChannel]]))
  #?(:clj (:import
           [clojure.core.async.impl.channels ManyToManyChannel])))

(defprotocol Task
  "A protocol for async values.

  (partial then task-val) is the sense of task described in
  https://github.com/leonoel/task."
  (then [this on-success on-failure]))

#?(:cljs
   (extend-protocol Task
     js/Promise
     (then [this on-success on-failure]
       (.then this on-success on-failure)
       #(c-warn "js/Promise cancellation not implemented."))))

#?(:clj
   (extend-protocol Task
     clojure.lang.IDeref
     (then [this on-success on-failure]
       (future (try (on-success @this)
                    (catch Throwable t
                      (on-failure t))))
       #(when (future? this) (future-cancel this)))))

(extend-protocol Task
  ManyToManyChannel
  (then [ch on-success _on-failure]
    (go (on-success (<! ch)))
    #(c-warn "core.async channel cancellation not implemented.")))

(defn >task [v]
  (if (fn? v) v (partial then v)))

(defmacro async-run! [& body]
  (if (:ns &env)
    `(js/Promise. (fn [res#] (try ~@body (finally (res# nil)))))
    `(future ~@body)))
