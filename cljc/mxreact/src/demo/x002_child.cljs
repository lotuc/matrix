(ns demo.x002-child
  (:require
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]))

(defn count-with-initial [{:keys [get-count]}]
  (mxr/button {:onClick #(if (mx/md-dead? me)
                           (js/console.log :dead)
                           (mx/mswap! me :count inc))}
    {:count (mx/cI (do (js/console.log :init-count-with-initial) (get-count)))
     :on-quiesce (fn [_] (js/console.log :quiesce-count-with-initial))}
    (str "click me to inc (initial count: " (mx/mget me :count) ")")))

(defn app []
  (mxr/div {} {:name :root :count (mx/cI 0)}
    (mxr/mx$ (str "count: " (mx/mget (mx/mpar) :count)))
    (mxr/$ :br)
    (mxr/button {:onClick #(mx/mswap! (mx/fmu :root) :count inc)} "click to inc")
    (mxr/$ :br)
    (mxr/span {} (count-with-initial {:get-count #(mx/mget (mx/fmu :root) :count)}))))
