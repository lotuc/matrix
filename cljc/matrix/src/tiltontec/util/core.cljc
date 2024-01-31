(ns tiltontec.util.core
  #?(:cljs (:require-macros [tiltontec.util.core :refer [pr-warn]]))
  (:require
   #?(:clj [tiltontec.util.base :refer [prog1]]
      :cljs [tiltontec.util.base :refer-macros [prog1]])))

(defn type-of [x] (type x))

(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(declare throw-ex)

(defn set-ify [x]
  (cond
    (nil? x) #{}
    (sequential? x) (set x)
    :else #{x}))

(defn cl-find [sought coll]
  (when-not (nil? sought)
    (some #{sought} coll)))

(def ^:dynamic *disable-warnings* false)

(comment
  #?(:clj (alter-var-root #'*disable-warnings* (constantly true))
     :cljs (set! *disable-warnings* true)))

(defmacro pr-warn [& args]
  ;; make sure we don't stackoverflow on printing recursive structures
  `(binding [*print-level* (or *print-level* 1)]
     (when-not *disable-warnings*
       #?(:clj (locking *out* (println (apply str "WARNING: " (list ~@args))))
          :cljs (js/console.log (apply str "WARNING: " (list ~@args)))))))

#?(:cljs
   (defn uuidv4 []
     (letfn [(hex [] (.toString (rand-int 16) 16))]
       (let [rhex (.toString (bit-or 0x8 (bit-and 0x3 (rand-int 16))) 16)]
         (uuid
          (str (hex) (hex) (hex) (hex)
               (hex) (hex) (hex) (hex) "-"
               (hex) (hex) (hex) (hex) "-"
               "4" (hex) (hex) (hex) "-"
               rhex (hex) (hex) (hex) "-"
               (hex) (hex) (hex) (hex)
               (hex) (hex) (hex) (hex)
               (hex) (hex) (hex) (hex)))))))

;; --- refs with maps conveniences -------------------

(defn ia-ref [x]
  (#?(:clj ref :cljs atom) x))

(defn any-ref? [x]
  (instance? #?(:cljs cljs.core.Atom
                :clj  clojure.lang.Ref) x))

(defn mut-set!
  ([mut prop new-value] (mut-set! mut prop new-value nil))
  ([mut prop new-value tag]
   (when-not (any-ref? mut)
     (pr-warn "model.util.core/mut-set!> prop:" prop :tag tag
              "new-value:" new-value
              "failed assertion any-ref? on ref:" mut)
     (assert false "see console"))
   (when-not (map? @mut)
     (pr-warn "model.util.core/mut-set!> prop:" prop :tag tag
              "new-value:" (or new-value :NIL)
              "failed assertion map? on ref:" @mut)
     (assert false "see console"))
   (#?(:clj alter :cljs swap!) mut assoc prop new-value)
   new-value))

(defn rmap-setf
  ([[prop ref] new-value]
   (rmap-setf [prop ref] new-value nil))
  ([[prop ref] new-value tag]
   (when-not (any-ref? ref)
     (pr-warn "model.util.core/rmap-setf> prop:" prop :tag tag
              "new-value:" new-value
              "failed assertion any-ref? on ref:" ref)
     (throw-ex "model.util.core/rmap-setf" {:ref ref :prop prop :tag tag}))
   (when-not (map? @ref)
     (pr-warn "model.util.core/rmap-setf> prop:" prop :tag tag
              "new-value:" (or new-value :NIL)
              "failed assertion map? on ref:" @ref)
     (throw-ex "model.util.core/rmap-setf" {:ref ref :prop prop :tag tag}))
   (#?(:clj alter :cljs swap!) ref assoc prop new-value)
   new-value))

(defn rmap-meta-setf [[prop ref] new-value]
  (assert (meta ref))
  (alter-meta! ref assoc prop new-value)
  new-value)

;; --- error handling -----------------

(defn throw-ex
  ([msg] (throw (ex-info msg {})))
  ([msg map] (throw (ex-info msg map)))
  ([msg map cause] (throw (ex-info msg map cause))))

;; --- deftest support ---------------------
;; These next two are lame because they just
;; look at props (ignoring models). Use only
;; in tests looking at one model or at least
;; prop names do not duplicate.
;;

(defn prop-users [me prop]
  (set (map :propq
            (map deref
                 (:callers @(prop @me) #{})))))

(defn prop-useds [me prop]
  (set (map :prop
            (map deref
                 (:useds @(prop @me) #{})))))

;;; --- FIFO Queue -----------------------------

(defn make-fifo-queue []
  (#?(:clj ref :cljs atom) []))

(defn fifo-data [q] @q)
(defn fifo-clear [q]
  (#?(:clj alter :cljs swap!) q empty))
(defn fifo-empty? [q]
  (empty? @q))
(defn fifo-peek [q]
  (first @q))
(defn fifo-add [q new]
  (#?(:clj alter :cljs swap!) q conj new))
(defn fifo-pop [q]
  (when-not (fifo-empty? q)
    (prog1
     (first @q)
     (#?(:clj alter :cljs swap!) q subvec 1))))

;;; --- detritus ----------------------

(defn ensure-vec [x]
  (if (coll? x) (vec x) [x]))

(defn now []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

;;; --- counting ----------------

(def counts (atom nil))

(defn counts-reset []
  (reset! counts nil))

(defn countit "Count it"
  ([path]
   (countit path 1))
  ([path n]
   (if (sequential? path)
     (if (counted? n)
       (countit path (count n))
       (swap! counts update-in path (fnil + 0) n))
     (countit [path] n))))

(comment
  (do (counts-reset)
      ;; (swap! counts update-in [:a :c] (fnil + 0) 1)
      (countit [:a :b] 7)
      (countit (list :a :b) 3)
      (countit :x 2)
      (countit :y [1 2 3 4])))
