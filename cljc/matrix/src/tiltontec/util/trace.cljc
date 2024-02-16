(ns tiltontec.util.trace
  #?(:cljs (:require-macros [tiltontec.util.trace :refer [wtrx trx pr-warn]]))
  #?(:clj (:require
           [clojure.string :as str])))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

;;; debug & trace related utilities

#?(:cljs (enable-console-print!))

(def ^:dynamic *trx?* true)

#_(alter-var-root #'*trx?* not)

(def ^:dynamic *disable-warnings* false)
(def ^:dynamic *trxdepth* 0)

(def +mx-sid+ (atom 0))

(defn mx-sid-next []
  (swap! +mx-sid+ inc))

(defn- call-trc$ [s bits]
  (str s ": " #?(:cljs (str bits) :clj (str/join ", " bits))))

(defn call-trc [s & os]
  ;; uncomment to escape loop
  ;; (break)
  (when *trx?*
    (when s
      (let [path (apply str (repeat *trxdepth* "."))]
        (println path (call-trc$ s os))))))

(defmacro trx [label & vals]
  `(call-trc ~(when (not (nil? label))
                (str label))
             ~@vals))

(defn call-wtrx [fn lo hi trxargs]
  (binding [*trxdepth* (inc *trxdepth*)
            *print-level* 3]
    (cond
      (<= lo *trxdepth* hi)
      (apply call-trc trxargs)
      (> *trxdepth* hi)
      (throw (#?(:cljs js/Error. :clj Exception.)
              (str "wtrx exceeded max depth " hi ":"
                   (call-trc$ (first trxargs) (rest trxargs))))))
    (fn)))

(defmacro wtrx [[lo hi & trxargs] & body]
  `(call-wtrx (fn [] ~@body) ~lo ~hi (list ~@trxargs)))

(defn prx [tag & bits]
  (when tag
    (binding [*print-level* 3]
      (apply prn tag bits))))

(comment
  #?(:clj (alter-var-root #'*disable-warnings* (constantly true))
     :cljs (set! *disable-warnings* true)))

(defmacro pr-warn [& args]
  ;; make sure we don't stackoverflow on printing recursive structures
  `(binding [*print-level* (or *print-level* 1)]
     (when-not *disable-warnings*
       #?(:clj (locking *out* (println (apply str "WARNING: " (list ~@args))))
          :cljs (js/console.log (apply str "WARNING: " (list ~@args)))))))
