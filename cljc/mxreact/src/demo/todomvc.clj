(ns demo.todomvc)

(defmacro swap-todos! [f & args]
  `(mx/mswap! (tiltontec.matrix.api/fmu :todo-list) :value ~f ~@args))
