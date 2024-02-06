(ns tiltontec.cell.watch
  #?(:cljs (:require-macros
            [tiltontec.util.ref :refer [rmap-set-prop!]]))
  (:require
   #?(:clj [tiltontec.util.ref :refer [rmap-set-prop!]])
   [tiltontec.cell.base :refer [*pulse* c-model c-prop c-ref? c-value unbound]]
   [tiltontec.cell.poly :refer [watch] :as poly]))

(defn c-watch
  ([c why]
   (c-watch c unbound why))
  ([c prior-value _why]
   (assert (c-ref? c))
   (assert (integer? @*pulse*))
   (rmap-set-prop! c :pulse-watched @*pulse*)
   (watch (c-prop c) (c-model c) (c-value c) prior-value c)
   (when-let [cw (:watch @c)]
     (cw (c-prop c) (c-model c) (c-value c) prior-value c))))
