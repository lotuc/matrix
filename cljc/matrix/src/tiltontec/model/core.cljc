(ns tiltontec.model.core
  #?(:cljs (:require-macros
            [tiltontec.model.core :refer [with-par mpar]]
            [tiltontec.util.ref :refer [dosync! make-ref ref-swap!
                                        meta-map-set-prop! ref-swap! rmap-set-prop!]]))
  (:require
   #?(:clj [tiltontec.util.ref
            :refer [dosync! make-ref meta-map-set-prop! ref-swap!
                    rmap-set-prop!]])
   [tiltontec.cell.base :refer [c-input? c-ref? c-value md-state unbound] :as cty]
   [tiltontec.cell.integrity :refer [with-integrity]]
   [tiltontec.cell.poly :refer [c-awaken md-awaken-before watch]]
   [tiltontec.util.trace :refer [mx-sid-next]]))

#?(:clj (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def matrix
  "Each app will populate this with the root of its application matrix."
  (atom nil))

(defn md-cz
  "`cz` is a map of prop-name (of model `me`) to cells."
  [me]
  (:cz (meta me)))

(defn md-cell
  "Return the cell for `prop` in model `me`."
  ([me prop] (prop (:cz (meta me))))
  ([me prop not-found] (prop (:cz (meta me)) not-found)))

(defn md-name [me]
  (:name @me))

(def ^:dynamic *parent* nil)

(defn- md-install-cell
  "Install a cell in a model, and return true if the cell is a `c-ref?`.
   If the cell is a ref, then the model will be installed as the
  cell's :me.

  note that c (a misnomer) might not be a Cell."
  [me prop c]
  (let [is-c-ref (c-ref? c)]
    (if is-c-ref
      (do
        (ref-swap! c assoc :prop prop :me me)
        (rmap-set-prop! me prop (when (c-input? c) (c-value c))))
      (rmap-set-prop! me prop c))
    is-c-ref))

(defn md-awaken
  "(1) do initial evaluation of all ruled props
   (2) call watchs of all props"
  [me]
  (assert me "md-awaken passed nil")
  (md-awaken-before me)
  (let [s (md-state me)]
    (assert (= :nascent s)
            (str "md-awaken> state not nascent post-awaken-before: " (or s :NIL) " meta: " (meta me))))
  (meta-map-set-prop! me ::cty/state :awakening)
  (doseq [prop (keys @me)]
    ;; next is tricky: if prop is in :cz but nil, it has been
    ;; optimized-away and watched then in the rare case
    ;; it gets optimized away on other than the initial value.
    ;;
    ;; Not sure this is about optimized away cells anymore (since optimized away
    ;; cell would go through `tiltontec.cell.evaluate/optimize-away?!` and
    ;; already been watched, is there any other optimization path?).
    ;;
    ;; But for constant (non-cell) property, the extra property `watch` step is
    ;; needed here.
    (if-some [c (md-cell me prop)]
      (c-awaken c)
      ;; these need at least an initial watch
      (watch prop me (prop @me) unbound nil)))
  (meta-map-set-prop! me ::cty/state :awake)
  me)

(defn make
  "Create a model whose parent is *parent*."
  [& args]
  (if (odd? (count args))
    (apply make :mx-type args)
    (dosync!
     (let [arg-map (apply hash-map args)
           mx-type (get arg-map :mx-type ::cty/model)
           meta-keys #{:mx-type :on-quiesce}
           props-map (apply dissoc arg-map meta-keys)

           me (make-ref
               (->> props-map
                    (map (fn [[k v]] [k (if (c-ref? v) unbound v)]))
                    (into {:parent *parent*}))
               :meta {::cty/state :nascent
                      :mx-sid     (mx-sid-next)
                      :mx-type    mx-type
                      :on-quiesce (get arg-map :on-quiesce)})
           cz (->> props-map
                   (filter (fn [[prop v]] (md-install-cell me prop v)))
                   (map vec)
                   (into {}))]

       (meta-map-set-prop! me :cz cz)

       (with-integrity [:awaken me]
         (md-awaken me))

       me))))

(defmacro with-par [m & body]
  `(binding [tiltontec.model.core/*parent* ~m]
     ~@body))

(defn md-par [me]
  (:parent @me))

(defmacro mpar [& [me]]
  (let [me (or me 'me)]
    `(:parent @~me)))
