(ns tiltontec.cell.synapse
  #?(:cljs (:require-macros
            [tiltontec.util.ref :refer [rmap-swap-prop!]]))
  (:require
   #?(:clj [tiltontec.cell.base
            :refer [*depender* c-model c-synapses dependency-record]]
      :cljs [tiltontec.cell.base
             :refer [*depender* c-synapses dependency-record]])
   #?(:clj [tiltontec.cell.core :refer [c-fn make-c-formula]]
      :cljs [tiltontec.cell.core :refer [make-c-formula]])
   #?(:clj [tiltontec.cell.diagnostic :refer [mxtrc]]
      :cljs [tiltontec.cell.diagnostic :refer-macros [mxtrc]])
   #?(:clj [tiltontec.util.ref :refer [rmap-swap-prop!]])
   [tiltontec.cell.evaluate :refer [ensure-value-is-current]]
   [tiltontec.cell.integrity]))

(defn existing-syn [synapse-id]
  (assert (keyword? synapse-id) "Synapse ID must be a keyword")
  (assert *depender* (str "You attempted to create synapse " synapse-id " outside a Cell formula. Synapses serve containing Cells."))
  (some #(when (= synapse-id (:synapse-id (deref %))) %)
        (c-synapses *depender*)))

(defmacro with-synapse [[synapse-id [& closure-bindings]] & body]
  `(let [existing-syn# (existing-syn ~synapse-id)
         synapse# (or existing-syn#

                      (let [new-syn#
                            (let [~@closure-bindings]
                              ;; (mxtrc :making-syn!? :depender (:prop @*depender*))
                              (make-c-formula
                               :model (:model @*depender*)
                               :prop ~synapse-id
                               :synapse-id ~synapse-id
                               :code '~body
                               :synaptic? true
                               :rule (c-fn ~@body)))]
                        ;; (mxtrc :built-synapse!!!!!!!!!!!!!!!! :synapse-id ~synapse-id :new-syn @new-syn#)
                        (rmap-swap-prop! *depender* :synapses conj  new-syn#)
                        (dependency-record new-syn#)        ;; needed?!!!!
                        ;; (mxtrc :made-syn!!!!!!!!!!!! :new-syn @new-syn#)
                        new-syn#))

         value# (tiltontec.cell.integrity/with-integrity ()
                  ;; (mxtrx :with-syn-ensure-syn-value :existing-syn-nil? (nil? existing-syn#))
                  (ensure-value-is-current synapse# :synapse *depender*))]
     ;; (mxtrx :synapse-returns :synapse-id ~synapse-id :useds (doall (map c-prop (c-useds synapse#))))
     ;; (cpr :syn-ret-value!!!!!! (map #(:uri (deref %)) value#))
     (dependency-record synapse#)
     value#))

(defn call-with-synapse [synapse-id cell-factory]
  (let [existing-syn (existing-syn synapse-id)
        synapse (or existing-syn
                    (let [new-syn (cell-factory)]
                      (mxtrc :building-synapse :synapse-id ~synapse-id)
                      (rmap-swap-prop! *depender* :synapses conj new-syn)
                      (dependency-record new-syn) ;; needed?!!!!
                      new-syn))

        value (tiltontec.cell.integrity/with-integrity ()
                (ensure-value-is-current synapse :synapse *depender*))]
    (dependency-record synapse)

    value))

(defmacro cSyn [[synapse-id [& closure-bindings]] & body]
  `(call-with-synapse ~synapse-id #(let [~@closure-bindings]
                                     (make-c-formula
                                      :model (c-model *depender*)
                                      :prop ~synapse-id
                                      :synapse-id ~synapse-id
                                      :code '~body
                                      :synaptic? true
                                      :rule (c-fn ~@body)))))
