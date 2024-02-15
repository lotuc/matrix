(ns tiltontec.model.navigate
  #?(:cljs (:require-macros [tiltontec.util.ref :refer [any-ref?]]))
  (:require
   #?(:clj [tiltontec.util.ref :refer [any-ref?]])
   #?(:clj [tiltontec.util.core
            :refer [pr-code-str throw-ex]]
      :cljs [tiltontec.util.core
             :refer [pr-code-str]
             :refer-macros [throw-ex]])
   [tiltontec.cell.base :refer [*depender* md-dead? without-c-dependency]]
   [tiltontec.cell.diagnostic :refer [minfo]]
   [tiltontec.model.accessors :refer [mget mget?]]))

(defn nextsib [mx]
  (without-c-dependency
   (loop [sibs (mget? (:parent @mx) :kids)]
     (when sibs
       (if (= mx (first sibs))
         (second sibs)
         (recur (rest sibs)))))))

(defn prevsib [mx]
  (without-c-dependency
   (loop [sibs (mget? (:parent @mx) :kids)]
     (when sibs
       (cond
         (= mx (first sibs)) nil
         (= mx (second sibs)) (first sibs)
         :else (recur (rest sibs)))))))

(defn- fm-navig=
  "Return true if 'poss' is the matrix reference we 'seek'

   There are 4 branches to this.

   'poss' is not a ref, return false
   'seek' is a fn?, we return result of invoke it with 'poss'
   'seek' is a keyword?, we return true if it is = with (:name poss)
   :else compare 'poss' and 'seek' directly using ="

  [seek poss]
  (cond
    (not (any-ref? poss)) false
    (fn? seek) (seek poss)
    (keyword? seek) (= seek (:name @poss))
    :else (= seek poss)))

(defn- fasc-higher [what where options]
  (or (and (:me? options) (fm-navig= what where) where)
      (when (any-ref? where)
        (recur what (:parent @where) (assoc options :me? true)))))

(defn fasc
  "Search matrix ascendents for 'what', starting at 'where'
   See fm-navig= for options about 'what' can be
   if :me? is true, and (fm-navig= what where) return 'where'
   if (:parent @where) returns a parent, recurse up the family tree
   return an error when (:must? options) is true and we nothing is found"
  [what where & options]
  (assert where (str "fasc> 'where' arg is nil seeking " what :options options))
  (assert (or (not (any-ref? where))
              (not (md-dead? where)))
          (str "fasc> pass dead 'where' " (minfo where) :seeking what))
  (assert what (str "fasc> 'what' arg is nil searching from " (minfo where) :options options))
  (let [options (merge {:me? false :wocd? true :must? true}
                       (apply hash-map options))]
    (binding [*depender* (if (:wocd? options) nil *depender*)]
      (or (fasc-higher what where options)
          (when (:must? options)
            (throw-ex "fasc: model not found"
                      {:what what :where where :options options}))))))

(defn fm-navig
  "Search matrix ascendents and descendents for `what`, starting at `where`.

   if `:me?` is true, and (fm-navig= what where) return
  'where' (`:me?` is `false` by default)

   if `:inside?` is true, try kids recursively (after removing any
  listed in `:skip` option)

   if `:up?` is true, invoke fm-navig on ancestor (skipping 'where')"
  [what where & options]
  (let [options (merge {:must? true :me? false, :inside? false, :up? true,
                              ;; without-c-dependency
                        :wocd? true}
                       (apply hash-map options))]
    (or (when (and where what (any-ref? where))
          (binding [*depender* (if (:wocd? options) nil *depender*)]
            (or (and (:me? options) (fm-navig= what where) where)

                (and (:inside? options)
                     (->> (mget? where :kids)
                          (remove #{(:skip options)})
                          (some #(fm-navig what %
                                           :must? false
                                           :me? true
                                           :inside? true
                                           :up? false))))

                (and (:up? options)
                     (when-let [par (:parent @where)]
                       (fm-navig what par
                                 :must? false
                                 :up? true
                                 :me? true
                                 :skip where
                                 :inside? true))))))
        (when (:must? options)
          (let [what-code$ (:what-code$ options)
                options (dissoc options :what-code$)
                d (cond-> (cond-> {:what what
                                   :where where
                                   :options options})
                    what-code$ (assoc :what-code$ what-code$))]
            (throw-ex "fm-navig: model not found" d))))))

(defn fm!
  "Search matrix ascendents and descendents from node 'where', for
  'what', throwing an error when not found"
  [what where]
  (fm-navig what where :me? false :inside? true :must? true :up? true))

(defmacro fmu
  "Search matrix ascendents from node 'me' (defaulting to 'me in current scope)
  looking for element with given name"
  [name & [me]]
  (let [me-ref (or me 'me)
        code$ (pr-code-str name)]
    `(let [name# ~name]
       (fm-navig #(= name# (mget? % :name)) ~me-ref
                 :me? false :up? true :inside? false
                 :what-code$ ~code$))))

(defmacro fmuinc
  "`fmu` with `me` inclusive."
  [name & [me]]
  (let [me-ref (or me 'me)
        code$ (pr-code-str name)]
    `(let [name# ~name]
       (fm-navig #(= name# (mget? % :name)) ~me-ref
                 :me? false :up? true :inside? false
                 :what-code$ ~code$))))

(defn- mxu-find-name
  "Search matrix ascendents from node 'where' looking for element with
  given name"
  [where name]
  (fm-navig #(= name (mget? % :name)) where
            :me? false :up? true :inside? false))

(defn- mxu-find-id
  "Search matrix ascendents from node 'where' looking for element with
  given id"
  [where id]
  (fm-navig #(= id (mget? % :id)) where
            :me? false :up? true :inside? false))

(defn fmo
  "Search matrix ascendents from node 'where' for `id-or-name`, trying first as
  a `name`, then as an `id`."
  [where id-or-name]
  (or (mxu-find-name where id-or-name)
      (mxu-find-id where id-or-name)
      (throw (ex-info (str "fmo> not id or name " id-or-name) {:id-name id-or-name}))))

(defn fmov
  "Use `fmo` and extract `:value` (or prop indicated by `:prop-name`)"
  ([where id-or-name]
   (fmov where id-or-name :value))
  ([where id-or-name prop-name]
   (when-let [mx (fmo where id-or-name)]
     (if (contains? @mx prop-name)
       (mget mx prop-name)
       (throw (ex-info (str "fmov> " id-or-name " lacks " prop-name " property")
                       {:id-name id-or-name :prop-name prop-name}))))))

(defmacro mdv!
  "Search matrix ascendents from node 'me' looking for `what`, and extract `prop`"
  [what prop & [me]]
  (let [me (or me 'me)]
    `(mget (tiltontec.model.navigate/fm! ~what ~me) ~prop)))
