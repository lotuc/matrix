(ns tiltontec.matrix.api
  #?(:cljs (:require-macros
            [tiltontec.matrix.api
             :refer [with-mx without-c-dependency cf-freeze the-kids cFkids with-par
                     fn-watch cF cF+ cFn cF+n cFonce cF1 with-cc mpar fmu mdv!
                     with-mx-trace with-minfo with-minfo-std
                     mxtrc with-integrity]]
            [tiltontec.util.ref :refer [any-ref1?]]))
  (:require
   #?(:clj [tiltontec.util.ref :refer [any-ref1?]])
   [tiltontec.cell.base :as cb]
   [tiltontec.cell.core]
   [tiltontec.cell.diagnostic :as diag]
   [tiltontec.model.accessors :as ma]
   [tiltontec.model.core :as md]
   [tiltontec.model.family :as mf]
   [tiltontec.model.navigate :as mn]
   [tiltontec.util.core :as ucore]
   [tiltontec.util.trace :as utrace]))

(defn prx [tag & bits] (apply utrace/prx tag bits))

(defn any-ref? [it] (any-ref1? it))

(def unbound cb/unbound)

;;; --- the matrix --------------------------------------------

(def matrix
  "Optionally populated with the root of a tree of Models."
  tiltontec.model.core/matrix)

(defmacro with-mx [& body]
  `(tiltontec.cell.core/call-with-mx
    (fn [] ~@body)))

;;;
(defn md-name [me] (:name @me))
(defn mname [me] (:name @me))
(defn mx-type [it] (ucore/mx-type it))
(defn md-state [it] (cb/md-state it))
(defn md-ref? [it] (cb/md-ref? it))
(defn md-dead? [it] (cb/mdead? it))

;;;--- cells ------------------------------------------

;;;;;; --- cell accessors ------------------------------------------

(defn c-value [c] (cb/c-value c))
(defn c-model [c] (cb/c-model c))
(defn c-prop-name [c] (cb/c-prop-name c))

;;; --- cell formula utilities ----------------------------------

(defmacro without-c-dependency [& body]
  `(tiltontec.cell.base/without-c-dependency ~@body))

(defmacro cf-freeze
  "Stop listening to dependents.
  Return the specified optional value, or the current latest value."
  [& [value-form]]
  `(tiltontec.cell.core/cf-freeze ~value-form))

;;; --- models ---------------------------------------

(defmacro def-mget [reader-prefix & props]
  `(tiltontec.model.accessors/def-mget ~reader-prefix ~@props))

;;; --- parent/kids ---------------------------------------------

(defmacro the-kids
  "Macro to flatten kids in `tree` and relate them to `me` via the *parent*
  dynamic binding"
  [& tree]
  `(tiltontec.model.family/the-kids ~@tree))

(defmacro cFkids
  "Syntax sugar for formulae that define :kids props"
  [& tree]
  `(tiltontec.model.family/cFkids ~@tree))

(defmacro with-par
  "Macro to bind *parent* to model `m` in `body`."
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

(defn cI [value & option-kvs]
  (apply tiltontec.cell.core/make-cell
    :value value
    :input? true
    option-kvs))

(defmacro cF [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '~body
    :rule (tiltontec.cell.core/c-fn ~@body)))

(defmacro cF+ [[& options] & body]
  `(tiltontec.cell.core/make-c-formula
    ~@options
    :code '~body
    :rule (tiltontec.cell.core/c-fn ~@body)))

(defmacro cFn [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? true
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cF+n [[& options] & body]
  `(tiltontec.cell.core/make-c-formula
    ~@options
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? true
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cFonce [& body]
  `(tiltontec.cell.core/make-c-formula
    :code '(tiltontec.cell.base/without-c-dependency ~@body)
    :input? nil
    :rule (tiltontec.cell.core/c-fn (tiltontec.cell.base/without-c-dependency ~@body))))

(defmacro cF1 [& body]
  `(tiltontec.cell.core/cFonce ~@body))

;;; --- model factory ----------------------------------------

(defn make [& arg-list]
  (apply tiltontec.model.core/make arg-list))

;;; --- mutation -----------------------------------

(defn mset! [me prop new-value]
  (ma/mset! me prop new-value))

(defn mswap! [me prop swap-fn & swap-fn-args]
  (apply ma/mswap! me prop swap-fn swap-fn-args))

(defn mget [me prop]
  (ma/mget me prop))

(defn mget? [me prop & [alt-value]]
  (ma/mget? me prop alt-value))

;;; --- integrity ---------------------------------

(defmacro with-integrity [[opcode info] & body]
  `(tiltontec.cell.integrity/with-integrity [~opcode ~info]
     ~@body))

(defmacro with-cc [id & body]
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

(defmacro as-ignored [expr]
  (list 'do (symbol "#_") expr))

(defmacro with-minfo-std [& body]
  `(binding [diag/*mx-minfo* nil]
     ~@body))

(defn minfo [me]
  (diag/minfo me))

(defn cinfo [c]
  (diag/cinfo c))
