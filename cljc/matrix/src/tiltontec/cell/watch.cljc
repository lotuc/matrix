(ns tiltontec.cell.watch
  (:require
   [tiltontec.cell.base
    :refer [*pulse* c-model c-prop c-ref? c-value unbound] :as cty]
   [tiltontec.cell.poly :refer [watch] :as poly]
   [tiltontec.util.core :as ucore]))

(defn c-watch
  ([c why]
   (c-watch c unbound why))
  ([c prior-value _why]
   (assert (c-ref? c))
   (assert (integer? @*pulse*))
   (ucore/rmap-setf [:pulse-watched c] @*pulse*)
   (watch (c-prop c) (c-model c) (c-value c) prior-value c)
   (when-let [cw (:watch @c)]
     (cw (c-prop c) (c-model c) (c-value c) prior-value c))))
