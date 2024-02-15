(ns tiltontec.cell.synapse
  (:require
   #?(:clj  [tiltontec.cell.core :refer [c-fn make-c-formula]]
      :cljs [tiltontec.cell.core :refer [make-c-formula] :refer-macros [c-fn]])
   #?(:clj  [tiltontec.util.ref :refer [rmap-swap-prop!]]
      :cljs [tiltontec.util.ref :refer-macros [rmap-swap-prop!]])
   [tiltontec.cell.base
    :refer [*depender* c-model c-synapses dependency-record]]
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
                              (make-c-formula
                               :model (c-model *depender*)
                               :prop ~synapse-id
                               :synapse-id ~synapse-id
                               :code '~body
                               :synaptic? true
                               :rule (c-fn ~@body)))]
                        (rmap-swap-prop! *depender* :synapses conj new-syn#)
                        new-syn#))
         value# (tiltontec.cell.integrity/with-integrity []
                  (ensure-value-is-current synapse#))]
     (dependency-record synapse#)
     value#))

