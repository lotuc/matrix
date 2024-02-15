(ns tiltontec.matrix.api
  #?(:cljs (:require-macros
            [tiltontec.matrix.api
             :refer [with-mx without-c-dependency cf-freeze the-kids cFkids with-par
                     fn-watch cF cF+ cFn cF+n cFonce cF1 with-cc mpar fmu mdv!
                     with-mx-trace with-minfo with-minfo-std
                     mxtrc with-integrity]]))
  (:require
   [tiltontec.cell.base :as cb]
   [tiltontec.cell.core]
   [tiltontec.cell.diagnostic :as diag]
   [tiltontec.model.accessors :as ma]
   [tiltontec.model.core :as md]
   [tiltontec.model.family :as mf]
   [tiltontec.model.navigate :as mn]
   [tiltontec.util.core :as ucore]))

(def unbound cb/unbound)

(def matrix
  "Optionally populated with the root of a tree of Models."
  tiltontec.model.core/matrix)

(defmacro with-mx [& body]
  `(tiltontec.cell.core/call-with-mx
    (fn [] ~@body)))

(defn mx-type
  "Return the type of the matrix reference `me`.

  A cell reference's type could be
  - `:tiltontec.cell.base/cell`
  - `:tiltontec.cell.base/c-formula` (which derives the `:tiltontec.cell.base/cell`)

  A model's reference type is given by `make`'s `mx-type` option
  - defaults to be `:tiltontec.cell.base/model`"
  [it]
  (ucore/mx-type it))

(defn mx-type?
  "Return true if `it`'s type (as returned by `mx-type`) is
  `type` (check with `isa?`)."
  [it type]
  (ucore/mx-type? it type))

(defn md-ref?
  "Return true if `me` is a model (reference)."
  [me]
  (cb/md-ref? me))

(defn md-name
  "Return the name of the model `me`."
  [me]
  (:name @me))

(defn md-state
  "Return the state of the model `me`.

  The state can be one of the following
  - :nascent
  - :awakening
  - :awake
  - :optimized-away
  - :dead"
  [me]
  (cb/md-state me))

(defn md-dead?
  "Return true if `md-state` of `me` is `:dead`."
  [me]
  (cb/md-dead? me))

;;; --- cells ---------------------------------------------

;;; cells can be standalone (not in a model) or in a model.

(defn c-value
  "Return the value of the cell `c`."
  [c]
  (cb/c-value c))

(defn c-model
  "Return the model of the cell `c` (possibly `nil`)."
  [c]
  (cb/c-model c))

(defn c-prop-name
  "Return the name of the property (of the cell model) of the cell
  `c` (possibly `nil`)."
  [c]
  (cb/c-prop-name c))

(defmacro without-c-dependency
  "Macro to execute `body` without establishing cell dependency."
  [& body]
  `(tiltontec.cell.base/without-c-dependency ~@body))

(defmacro cf-freeze
  "Stop listening to dependents.

  Return the specified optional value, or the current latest value."
  [& [value-form]]
  `(tiltontec.cell.core/cf-freeze ~value-form))

(defmacro def-mget
  "Define a reader function for a model's property."
  [reader-prefix & props]
  `(tiltontec.model.accessors/def-mget ~reader-prefix ~@props))

;;; --- parent/kids ---------------------------------------------

(defmacro the-kids
  "Macro to flatten kids in `tree` and relate them to `me` via the
  `tiltontec.model.core/*parent*` dynamic binding"
  [& tree]
  `(tiltontec.model.family/the-kids ~@tree))

(defmacro cFkids
  "Syntax sugar for formulae that define :kids props"
  [& tree]
  `(tiltontec.model.family/cFkids ~@tree))

(defmacro with-par
  "Macro to bind `tiltontec.model.core/*parent*` to model `m` in
  `body`."
  [m & body]
  `(tiltontec.model.core/with-par ~m ~@body))

(defn kid-values-kids
  "A pattern commonly employed in matrix applications is to define
  a :kid-factory on some 'parent' cell, and use it to enrich the value
  extracted from the parent's kid cells.

   This function maps across the :kids-values, invoking the factory as
  it goes.

  ```clojure
  (cF (make :some-mx-type
        :kid-values some-sequence-value
        ;;
        ;; the built kid is cached by sequence item's value,
        ;; `:kid-key` is a function retrieves the items'value from built children.
        ;;
        :kid-key (fn [child] (mx/mget child :your-key-prop))
        :kid-factory (fn [me item-val]
                       (make :your-child-type
                         :your-key-prop item-val
                         ...))
        (kid-values-kids me _cache)))
  ```"
  [me existing-kids]
  (mf/kid-values-kids me existing-kids))

;;; --- watch -----------------------------------------

(defmacro fn-watch
  "Shortcut definer for cell-specific watchs.

  body can be multiple sexprs with access to call parameters: `prop`,
  `me`, `new`, `old`, and `c`."
  [& body]
  `(fn [~'prop ~'me ~'new ~'old ~'c]
     ~@body))

;;; --- cell factories -----------------------------------------

(defn cI
  "Create a `Input` cell with the given initial `value` and cell options."
  [value & options]
  (apply tiltontec.cell.core/make-cell
    :value value
    :input? true
    options))

(defmacro cF
  "Create a `Formula` cell with formula `body`.

  the `body` will be executed with following symbol bound:
  - `me`: the cell's associated model (`c-model`)
  - `_prop-name`: the cell's property name (`c-prop-name`)
  - `_cache`: the cell's current value (`c-value`)
  - `_cell`: the cell itself"
  [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '~body
    :rule (tiltontec.cell.core/c-fn ~@body)))

(defmacro cF+
  "A version `cF` that takes extra cell options."
  [[& options] & body]
  `(tiltontec.cell.core/make-c-formula
    ~@options
    :code '~body
    :rule (tiltontec.cell.core/c-fn ~@body)))

(defmacro cFn
  "Start as formula for initial value computation, then convert to input cell."
  [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? true
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cF+n
  "A version `cFn` that takes extra cell options."
  [[& options] & body]
  `(tiltontec.cell.core/make-c-formula
    ~@options
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? true
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cFonce
  "Start as formula for initial computation, then behave as immutable property."
  [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? nil
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cF1
  "Alias to `cFonce`."
  [& body]
  `(tiltontec.cell.core/cFonce ~@body))

;;; --- model factory ----------------------------------------

(defn make
  "Create a model whose parent is `tiltontec.model.core/*parent*`.

  When `arg-list` is odd, convert the first element to value of `:mx-type`.

  - `:mx-type` is the type of the model, defaulting to `:tiltontec.cell.base/model`.
  - `:on-quiesce` is a function to be called when the model quiesces.
  - The rest of the elements are treated as key-value pairs to be used
  as properties of the model."
  [& arg-list]
  (apply tiltontec.model.core/make arg-list))

;;; --- mutation -----------------------------------

(defn mset!
  "Set the `prop`'s value of model `me` to be `new-value`. Returns the
  new-value."
  [me prop new-value]
  (ma/mset! me prop new-value))

(defn mswap!
  "Swaps the `prop`'s value of model `me` to be
  (apply swap-fn current-value-of-prop args)."
  [me prop swap-fn & swap-fn-args]
  (apply ma/mswap! me prop swap-fn swap-fn-args))

(defn mget
  "Get the `prop`'s value of model `me`. Throw exception if no `prop`
  found in model `me`."
  [me prop]
  (ma/mget me prop))

(defn mget?
  "Get the `prop`'s value of model `me`. Returns `alt-value` if no
  `prop` found in model `me`."
  [me prop & [alt-value]]
  (ma/mget? me prop alt-value))

;;; --- integrity ---------------------------------

(defmacro with-integrity [[opcode info] & body]
  `(tiltontec.cell.integrity/with-integrity [~opcode ~info]
     ~@body))

(defmacro with-cc
  [id & body]
  `(tiltontec.cell.integrity/with-integrity [:change ~id]
     ~@body))

;;; --- navigation ---------------------------------

(defmacro mpar [& [me]]
  (let [me (or me 'me)]
    `(:parent @~me)))

(defn fm-navig [what where & options]
  (apply mn/fm-navig what where options))

(defn fasc
  "Search up from `where`, excluding where and following only parent
  links for `what`."
  [what where & options]
  (apply mn/fasc what where options))

(defmacro fmu
  "Search matrix ascendents from node 'me' (defaulting to 'me in current scope)
  looking for element with given name"
  [name & [_me :as options]]
  `(tiltontec.model.navigate/fmu ~name ~@options))

(defmacro fmuinc
  "`fmu` inclusive of the starting node `me`."
  [name & [_me :as options]]
  `(tiltontec.model.navigate/fmuinc ~name ~@options))

(defn fm!
  "Search matrix ascendents and descendents from node 'where', for
  'what', throwing an error when not found"
  [what where]
  (mn/fm! what where))

(defn mxu-find-type
  "Search matrix ascendants from node `me` for first with given tag"
  [me type]
  (assert me)
  (fasc (fn [visited] (= type (mx-type visited))) me))

(defmacro mdv!
  "Search matrix ascendents from node 'me' looking for `what`, and
  extract `slot`"
  [what slot & [_me :as options]]
  `(tiltontec.model.navigate/mdv! ~what ~slot ~@options))

;;; --- debug --------------------------

(defmacro with-mx-trace [target & body]
  `(binding [diag/*mx-trace* ~target]
     ~@body))

(defmacro mxtrc [tag & bits]
  `(diag/mxtrc ~tag ~@bits))

(defmacro with-minfo [minfo-body & body]
  `(binding [diag/*mx-minfo* (fn [~'me] ~minfo-body)]
     ~@body))

(defmacro with-minfo-std [& body]
  `(binding [diag/*mx-minfo* nil]
     ~@body))

(defn minfo [me]
  (diag/minfo me))

(defn cinfo [c]
  (diag/cinfo c))
