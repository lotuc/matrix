(ns demo.x007-async
  (:require
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :as mx]))

(defn delay-1s [task-id]
  (fn [ok _err]
    (let [timeout-id (js/setTimeout
                      #(do (js/console.log "finish delay-1s" task-id)
                           (ok (str "finished " task-id)))
                      1000)]
      #(do (js/console.log "cancel delay-1s" task-id)
           (js/clearTimeout timeout-id)))))

(defn app []
  (mxr/span {:style {:display "flex" :flexDirection "column" :alignItems "flex-start"}}
    (mx/make
      :name :task
      :task-id (mx/cI 0)
      :abort-last? (mx/cI false)
      :pending-value (mx/cI nil)
      :keep-last? (mx/cI false))

    (mxr/div {:style {:display "flex" :flexDirection "column" :alignItems "flex-start"}}
      (mxr/span {}
        "abort last task on new async value: "
        (mxr/input {:type "checkbox"
                    :value (mx/mget me :value)
                    :onChange #(mx/mset! (mx/fmu :task) :abort-last? (.. % -target -checked))}
          {:value (mx/cF (mxr/fmu-val :task :abort-last?))}))
      (mxr/span {}
        "keep last on pending task: "
        (mxr/input {:type "checkbox"
                    :value (mx/mget me :value)
                    :onChange #(if (.. % -target -checked)
                                 (doto (mx/fmu :task)
                                   (mx/mset! :keep-last? true)
                                   (mx/mset! :pending-value nil))
                                 (mx/mset! (mx/fmu :task) :keep-last? false))}
          {:value (mx/cF (mxr/fmu-val :task :keep-last?))}))
      (mxr/span {}
        "pending value: "
        (mxr/input {:value (or (mx/mget me :value) "")
                    :disabled (mx/mget me :disabled)
                    :onChange #(let [v (.. % -target -value)]
                                 (if (zero? (count v))
                                   (mx/mset! (mx/fmu :task) :pending-value nil)
                                   (mx/mset! (mx/fmu :task) :pending-value v)))}
          {:value (mx/cF (mxr/fmu-val :task :pending-value))
           :disabled (mx/cF (mxr/fmu-val :task :keep-last?))})))

    (mxr/button {:onClick (fn [] (mx/mswap! (mx/fmu :task) :task-id inc))}
      {:task-id (mx/cF (mxr/fmu-val :task :task-id))}
      "click me to start task (" (mx/mget me :task-id) ")")

    (mxr/span {}
      {:pending-value (mx/cF (mxr/fmu-val :task :pending-value))
       :abort-last? (mx/cF (mxr/fmu-val :task :abort-last?))
       :keep-last? (mx/cF (mxr/fmu-val :task :keep-last?))}
      (let [pending-value (mx/mget me :pending-value)
            abort-last? (mx/mget me :abort-last?)
            keep-last? (mx/mget me :keep-last?)]
        (if (some? pending-value)
          (mxr/span {:style {:color "green"}}
            {:task-id (mx/cF (mxr/fmu-val :task :task-id))
             :task-result (mx/cF+ [:async? {:abort-last? abort-last?
                                            :pending-value pending-value
                                            :keep-last? keep-last?}]
                            (delay-1s (mx/mget me :task-id)))}
            "[ " (mx/mget me :task-result) " ]")
          (mxr/span {:style {:color "green"}}
            {:task-id (mx/cF (mxr/fmu-val :task :task-id))
             :task-result (mx/cF+ [:async? {:abort-last? abort-last?
                                            :keep-last? keep-last?}]
                            (delay-1s (mx/mget me :task-id)))}
            "[ " (mx/mget me :task-result) " ]"))))))
