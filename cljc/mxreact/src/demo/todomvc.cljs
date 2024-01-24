(ns demo.todomvc
  (:require-macros [demo.todomvc :refer [swap-todos!]])
  (:require
   [clojure.string :as string]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :refer-macros [fmu cFonce] :as mx]))

;; basically one-to-one copy of
;;; https://github.com/tastejs/todomvc/tree/efafb5843a52fad8b465f642ee8d5d980ad1ac4d/examples/react

(def url-alphabet "useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict")

(defn- nanoid [& {:keys [size] :or {size 21}}]
  (let [n (count url-alphabet)]
    (string/join (for [_ (range size)]
                   (get url-alphabet (rand-int n))))))

(defn- sanitize [s]
  (->> {"&" "&amp;"
        "<" "&lt;"
        ">" "&gt;"
        "\"" "&quot;"
        "'" "&#x27;"
        "/" "&#x2F;"}
       (reduce (fn [r [k v]] (string/replace r k v)) s)))

(comment
  (sanitize "a & b & c / d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; model

(defn- add-todo [todos title]
  (conj todos {:id (nanoid) :title title :completed? false}))

(defn- update-todos [todos id title]
  (->> todos
       (map (fn [v] (cond-> v (= (:id v) id) (assoc :title title))))
       (into [])))

(defn- toggle-todo [todos id]
  (->> todos
       (map (fn [v] (cond-> v (= (:id v) id) (update :completed? not))))
       (into [])))

(defn- toggle-all [todos completed?]
  (->> todos
       (map (fn [v] (assoc v :completed? completed?)))
       (into [])))

(defn- remove-todos [todos id]
  (->> todos
       (filter (fn [v] (not= (:id v) id)))
       (into [])))

(defn- remove-completed [todos]
  (->> todos
       (filter (complement :completed?))
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn Input [{:keys [placeholder default-value label on-submit on-blur]}]
  (mxr/div {} {:className "input-container"}
           (mxr/input {}
                      (cond-> {:className "new-todo" :id "todo-input" :type "text"
                               :autoFocus true
                               :onBlur (fn [_] (when on-blur (on-blur)))
                               :onKeyDown #(when (= "Enter" (.-key %))
                                             (let [v (.trim (.-value (.-target %)))]
                                               (when (>= (count v) 2)
                                                 (on-submit (sanitize v))
                                                 (set! (.-value (.-target %)) ""))))}
                        placeholder (assoc :placeholder placeholder)
                        default-value (assoc :defaultValue default-value)))
           (mxr/label {} {:className "visually-hidden"} label)))

(defn header []
  (mxr/header
   {} {:className "header"}
   (mxr/h1 {} {} "todos")
   (Input {:on-submit
           ;; macro magic which is equivelant to
           ;; #(mx/mswap! (fmu :todo-list) :value add-todo %)
           ;;
           ;; the following demo won't use it, but it shows the verbosity can be
           ;; abstracted away, and it is pretty local scoped.
           #(swap-todos! add-todo %)

           :label "New Todo Input"
           :placeholder "What needs to be done?"})))

(defn item [index {:keys [id title completed?]}]
  (mxr/li {:key index} (if completed? {:className "completed"} {})
          (mxr/div {:name :item :writable? (mx/cI false)}
                   {:className "view"}
                   (let [c me]
                     (if (mx/mget me :writable?)
                       (Input {:on-blur #(mx/mswap! c :writable? not)
                               :default-value title
                               :on-submit #(do (if (= (count %) 0)
                                                 (mx/mswap! (fmu :todo-list) :value remove-todos id)
                                                 (mx/mswap! (fmu :todo-list) :value update-todos id %))
                                               (mx/mswap! c :writable? not))})
                       [(mxr/input {} {:className "toggle" :type "checkbox" :checked completed?
                                       :onChange (fn [_] (mx/mswap! (fmu :todo-list) :value toggle-todo id))})
                        (mxr/label {} {:className "todo-item-label"
                                       :onDoubleClick (fn [_] (mx/mswap! c :writable? not))} title)
                        (mxr/button {} {:className "destroy"
                                        :onClick #(mx/mswap! (fmu :todo-list) :value remove-todos id)})])))))

(defn main []
  (mxr/main {:name :main
             :visible-todos (mx/cF (let [route-hash (mx/mget (fmu :router) :hash)
                                         todos (mx/mget (fmu :todo-list) :value)]
                                     (filter
                                      (case route-hash
                                        "#/active" (complement :completed?)
                                        "#/completed" :completed?
                                        (constantly true))
                                      todos)))
             :some-visible? (mx/cF (pos? (count (mx/mget me :visible-todos))))}
            {:className "main"}
            (when (mx/mget me :some-visible?)
              (mxr/div {} {:className "toggle-all-container"}
                       (mxr/input {}
                                  {:className "toggle-all" :type "checkbox"
                                   :defaultChecked (every? :completed? (mx/mget (fmu :main) :visible-todos))
                                   :onChange #(mx/mswap! (fmu :todo-list) :value toggle-all (.-checked (.-target %)))})
                       (mxr/label {} {:className "toggle-all-label"} "Toggle All Input")))
            (mxr/ul {:kid-values (mx/cF (mx/mget (fmu :main) :visible-todos))
                     :kid-key #(mx/mget % :key)
                     :kid-factory item}
                    {:className "todo-list"}
                    (mx/kid-values-kids me _cache))))

(defn footer []
  (mxr/footer
   {:name :footer
    :counts (mx/cF (let [todos (mx/mget? (fmu :todo-list) :value)]
                     {:total (count todos)
                      :active (count (filter (complement :completed?) todos))}))}
   {:className "footer"}

   (mxr/span {} {:className "todo-count"}
             (let [c (:active (mx/mget (fmu :footer) :counts))]
               (str c " " (if (<= c 1) "item" "items") " left")))
   (mxr/ul {} {:className "filters"}
           (for [[href txt]  [["#/" "All"] ["#/active" "Active"] ["#/completed" "Completed"]]
                 :let [selected? (= href (mx/mget (fmu :router) :hash))
                       prop (merge (when selected? {:className "selected"}) {:href href})]]
             (mxr/li {} {} (mxr/a {} prop txt))))

   (mxr/button {:disabled? (mx/cF (let [{:keys [total active]} (mx/mget (fmu :footer) :counts)]
                                    (= total active)))}
               {:className "clear-completed"
                :disabled (mx/mget me :disabled?)
                :onClick (fn [_] (mx/mswap! (fmu :todo-list) :value remove-completed))}
               "Clear completed")))

(defn demo []
  (mx/make
   :todomvc
   :rx-dom
   (cFonce
    (mxr/div {:name :todo-list :value (mx/cI [])} {:className "todoapp"}
              ;; non-element kid
             (mx/make ::router
                      :name :router
                      :on-quiesce (fn [c]
                                    (js/window.navigation.removeEventListener
                                     "navigate" (mx/mget c :listener)))
                      :listener (mx/cFonce
                                 (let [cb (fn [e] (mx/mset! me :hash (.-hash (js/URL. (.-url (.-destination e))))))]
                                   (js/window.navigation.addEventListener "navigate" cb)
                                   cb))
                      :hash (mx/cI js/window.location.hash))
              ;; react-element kid
             (header)
             (main)
             (footer)))))
