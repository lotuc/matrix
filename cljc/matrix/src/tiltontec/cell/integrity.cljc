(ns tiltontec.cell.integrity
  #?(:cljs (:require-macros
            [tiltontec.cell.integrity
             :refer [with-integrity with-cc without-integrity with-async-change]]
            [tiltontec.util.ref :refer [ref-swap!]]))
  (:require
   #?(:cljs [tiltontec.util.trace :refer-macros [trx]]
      :clj  [tiltontec.util.trace :refer [trx]])
   #?(:clj [tiltontec.util.ref :refer [ref-swap!]])
   #?(:clj [tiltontec.util.core
            :refer [fifo-add fifo-peek fifo-pop prog1 throw-ex]]
      :cljs [tiltontec.util.core
             :refer [fifo-add fifo-peek fifo-pop throw-ex]
             :refer-macros [prog1]])
   [tiltontec.cell.base
    :refer [*defer-changes* *dp-log* *one-pulse?* *pulse*
            *unfinished-business* *within-integrity* +client-q-handler+
            c-optimized-away? c-pulse un-stopped]]))

;; --- the pulse ------------------------------

(defn data-pulse-next
  ([] (data-pulse-next :anon))
  ([pulse-info]
   (when-not *one-pulse?*
     (when *dp-log*
       (trx "dp-next> " (inc @*pulse*) pulse-info))
     (ref-swap! *pulse* inc))))            ;; hhack try as commute

(defn c-current? [c]
  (when-some [p (c-pulse c)]
    (= p @*pulse*)))

(defn c-pulse-update [c _key]
  (when-not (c-optimized-away? c)
    (let [cell-pulse (c-pulse c)
          pulse @*pulse*]
      (assert (or (nil? cell-pulse) (>= @*pulse* cell-pulse)))
      (ref-swap! c assoc :pulse pulse))))

;; --- ufb utils ----------------------------

(defn ufb-counts []
  (into {} (for [[k v] *unfinished-business*]
             [k (count @v)])))

(defn ufb-queue [opcode]
  (or (opcode *unfinished-business*)
      (throw-ex "ufb-queue> opcode unknown" {:opcode opcode})))

(defn ufb-add [opcode continuation]
  (fifo-add (ufb-queue opcode) continuation))

(defn ufb-peek [opcode]
  (fifo-peek (ufb-queue opcode)))

(defn ufb-pop [opcode]
  (fifo-pop (ufb-queue opcode)))

(defn ufb-assert-q-empty [opcode]
  (if-let [uqp (fifo-peek (ufb-queue opcode))]
    (throw-ex "ufb queue not empty" {:opcode opcode :uqp uqp})
    true))

(defn ufb-do
  ([opcode]
   (ufb-do (ufb-queue opcode) opcode))

  ([q opcode]
   (when-let [[defer-info task] (fifo-pop q)]
     (trx nil :ufb-do-yep defer-info task)
     (task opcode defer-info)
     (recur q opcode))))

;; --- the ufb and integrity beef ----------------------
;;    proper ordering of state propagation

(defn finish-business []
  (un-stopped
   (loop [tag :tell-dependents]
     (case tag
       :tell-dependents
       (do (ufb-do :tell-dependents)
           (ufb-do :awaken)
           (recur
            (if (ufb-peek :tell-dependents)
              :tell-dependents
              :handle-clients)))

       :handle-clients
       (when-let [clientq (ufb-queue :client)]
         (if-let [cqh @+client-q-handler+]
           (cqh clientq)
           (ufb-do clientq :client))
         (recur
          (if (ufb-peek :client)
            :handle-clients
            :ephemeral-reset)))

       :ephemeral-reset
       (do (ufb-do :ephemeral-reset)
           (recur :deferred-state-change))

       :deferred-state-change
       (when-let [[defer-info task-fn] (ufb-pop :change)]
         (data-pulse-next :defferred-state-chg)
         (task-fn :change defer-info)
         (recur :tell-dependents))))))

(declare call-with-integrity)

(defmacro with-integrity [[opcode info] & body]
  `(tiltontec.cell.integrity/call-with-integrity
    ~opcode
    ~info
    (fn [~'opcode ~'defer-info]
      ~@body)))

(defmacro with-cc [id & body]
  `(with-integrity [:change ~id]
     ~@body))

(defmacro without-integrity [& body]
  `(binding
    [*within-integrity* false
     *defer-changes* false
     *call-stack* '()]
     ~@body))

(defmacro with-async-change [id & body]
  `(binding
    [*within-integrity* false
     *defer-changes* false
     *call-stack* '()]
     (with-integrity [:change ~id]
       ~@body)))

(defn call-with-integrity
  [opcode defer-info action]
  (#?(:cljs do :clj dosync)
   (if *within-integrity*
     (if opcode
       (prog1
        :deferred-to-ufb-1
         ;; SETF is supposed to return the value being installed
         ;; in the place, but if the SETF is deferred we return
         ;; something that will help someone who tries to use
         ;; the setf'ed value figure out what is going on:
        (ufb-add opcode [defer-info action]))

       ;; thus by not supplying an opcode one can get something
       ;; executed immediately, potentially breaking data integrity
       ;; but signifying by having coded the with-integrity macro
       ;; that one is aware of this.
       ;;
       ;; If you have read this comment.
       ;;
       (action opcode defer-info))

     (binding [*within-integrity* true
               *defer-changes* false]
       (when (or (zero? @*pulse*)
                 (= opcode :change))
         (data-pulse-next [:cwi opcode defer-info]))

       (prog1
        (action opcode defer-info)
        (finish-business)
        (ufb-assert-q-empty :tell-dependents)
        (ufb-assert-q-empty :change))))))
