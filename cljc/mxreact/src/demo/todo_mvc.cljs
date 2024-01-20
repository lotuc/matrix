(ns demo.todo-mvc
  (:require
   [mxreact.mxreact
    :refer [div h1 header input li ul]]
   [react]
   [tiltontec.matrix.api
    :refer-macros [cFonce fmu]
    :refer [cI make mget mset! mswap!]]))

(defn demo []
  (make ::todo-mvc
        :rx-dom
        (cFonce (div {:name :todo-list
                      :value (cI [])}
                     {}
                     (header {} {:className "header"} (h1 {} {} "todos"))
                     (input {:value (cI "")}
                            {:className "new-todo"
                             :placeholder "What needs to be done?"
                             :value (mget me :value)
                             :onChange #(mset! me :value (.-value (.-target %)))
                             :onKeyDown (fn [e]
                                          (when (= "Enter" (.-key e))
                                            (let [v (mget me :value)]
                                              (mset! me :value "")
                                              (mswap! (fmu :todo-list) :value conj v))))})
                     (div {} {} (str (count (mget (fmu :todo-list) :value)) " things to be done:"))
                     (ul {} {}
                         (map (fn [v] (li {} {} (str v)))
                              (mget (fmu :todo-list) :value)))))))
