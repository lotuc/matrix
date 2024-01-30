(ns tiltontec.model.base
  {:clj-kondo/ignore [:redundant-do]}
  (:require
   #?(:clj  [clojure.test :refer :all])
   #?(:cljs [tiltontec.util.base :refer-macros [def-rmap-props def-rmap-meta-props]]
      :clj [tiltontec.util.base :refer [def-rmap-meta-props def-rmap-props]])
   [tiltontec.cell.base :refer [c-input? c-ref? c-value md-state unbound] :as cty]
   [tiltontec.cell.poly :refer [c-awaken md-awaken-before watch]]
   [tiltontec.util.core :refer [rmap-meta-setf rmap-setf]]))

(def-rmap-props md- name)

;;; we let cell.base define md-state
(def-rmap-meta-props md- cz)

(defn md-cell [me prop]
  (prop (:cz (meta me))))

;;; --- md initialization ---

(defn md-install-cell
  "Install a cell in a model, and return true if the cell is a `c-ref?`.
   If the cell is a ref, then the model will be installed as the
  cell's :me.

  note that c (a misnomer) might not be a Cell."
  [me prop c]
  (let [is-c-ref (c-ref? c)]
    (if is-c-ref
      (do
        (#?(:clj alter :cljs swap!) c assoc :prop prop :me me)
        (rmap-setf [prop me] (when (c-input? c) (c-value c))))
      (rmap-setf [prop me] c))
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
  (rmap-meta-setf [::cty/state me] :awakening)
  (doseq [prop (keys @me)]
    ;; next is tricky: if prop is in :cz but nil, it has been
    ;; optimized-away and watched then in the rare case
    ;; it gets optimized away on other than the initial value.
    (when-let [c (prop (md-cz me) :not-found)]
      (if (= c :not-found)
        ;; these need at least an initial watch
        (watch prop me (prop @me) unbound nil)
        (c-awaken c))))
  (rmap-meta-setf [::cty/state me] :awake)
  me)
