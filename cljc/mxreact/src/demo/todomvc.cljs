(ns demo.todomvc
  (:require
   [clojure.string :as string]
   [mxreact.mxreact :as mxr]
   [react]
   [tiltontec.matrix.api :refer-macros [fmu] :as mx]))

;; basically one-to-one copy of
;;; https://github.com/tastejs/todomvc/tree/efafb5843a52fad8b465f642ee8d5d980ad1ac4d/examples/react

(def url-alphabet "useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict")

(defn- nanoid [& {:keys [size] :or {size 21}}]
  (let [n (count url-alphabet)]
    (string/join (for [_ (range size)] (get url-alphabet (rand-int n))))))

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
;;; todo model

(defn- add-todo [todos title]
  (conj todos {:id (nanoid) :title title :completed? false}))

(defn- update-todos [todos id title]
  (into [] (map (fn [v] (cond-> v (= (:id v) id) (assoc :title title))) todos)))

(defn- toggle-todo [todos id]
  (into [] (map (fn [v] (cond-> v (= (:id v) id) (update :completed? not))) todos)))

(defn- toggle-all [todos completed?]
  (into [] (map (fn [v] (assoc v :completed? completed?)) todos)))

(defn- remove-todos [todos id]
  (into [] (filter (fn [v] (not= (:id v) id)) todos)))

(defn- remove-completed [todos]
  (into [] (filter (complement :completed?) todos)))

(defn- make-todo [parent] (mx/with-par parent (mx/make ::todo :name :todo-list :value (mx/cI []))))
(defn- swap-todo! [me f & args] (apply mx/mswap! (fmu :todo-list me) :value f args))
(defn- todo-list [me] (mx/mget (fmu :todo-list me) :value))
(defn- add-todo! [me title] (swap-todo! me add-todo title))
(defn- update-todos! [me id title] (swap-todo! me update-todos id title))
(defn- toggle-todo! [me id] (swap-todo! me toggle-todo id))
(defn- toggle-all! [me completed?] (swap-todo! me toggle-all completed?))
(defn- remove-todos! [me id] (swap-todo! me remove-todos id))
(defn- remove-completed! [me] (swap-todo! me remove-completed))

;;; router model
(defn make-router [parent]
  (mx/with-par parent
    (mx/make ::router
      :name :router
      :on-quiesce (fn [me]
                    (js/window.navigation.removeEventListener
                     "navigate" (mx/mget me :listener)))
      :listener (mx/cFonce
                  (let [cb (fn [e] (mx/mset! me :hash (.-hash (js/URL. (.-url (.-destination e))))))]
                    (js/window.navigation.addEventListener "navigate" cb)
                    cb))
      :hash (mx/cI js/window.location.hash))))
(defn router-hash [me] (mx/mget (fmu :router me) :hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn Input [{:keys [placeholder default-value label on-submit on-blur]}]
  (mxr/div {:className "input-container"}
    (mxr/input (cond-> {:className "new-todo" :id "todo-input" :type "text"
                        :autoFocus true
                        :onBlur (fn [_] (when on-blur (on-blur)))
                        :onKeyDown #(when (= "Enter" (.-key %))
                                      (let [v (.trim (.-value (.-target %)))]
                                        (when (pos? (count v))
                                          (on-submit (sanitize v))
                                          (set! (.-value (.-target %)) ""))))}
                 placeholder (assoc :placeholder placeholder)
                 default-value (assoc :defaultValue default-value)))
    (mxr/label {:className "hidden"} label)))

(defn Header []
  (mxr/header
    {:className "header"}
    (mxr/h1 {} "todos")
    (Input {:on-submit
            (partial add-todo! me)
            :label "New Todo Input"
            :placeholder "What needs to be done?"})))

(defn Item [{:keys [id title completed?]}]
  (mxr/div {:className "view"}
    {:writable? (mx/cI false)}
    (let [toggle-wriable? #(mx/mswap! me :writable? not)]
      (if (mx/mget me :writable?)
        (Input {:on-blur toggle-wriable?
                :default-value title
                :on-submit #(do (if (= (count %) 0)
                                  (remove-todos! me id)
                                  (update-todos! me id %))
                                (toggle-wriable?))})
        [(mxr/input {:className "toggle" :type "checkbox" :checked completed?
                     :onChange #(toggle-todo! me id)})
         (mxr/label {:className "todo-item-label"
                     :onDoubleClick toggle-wriable?}
           title)
         (mxr/button {:className "destroy"
                      :onClick #(remove-todos! me id)})]))))

(defn Main []
  (mxr/main {:className "main"}
    {:name :main
     :visible-todos (mx/cF (let [route-hash (router-hash me)
                                 todos (todo-list me)]
                             (filter
                              (case route-hash
                                "#/active" (complement :completed?)
                                "#/completed" :completed?
                                (constantly true))
                              todos)))
     :some-visible? (mx/cF (pos? (count (mx/mget me :visible-todos))))}
    (when (mx/mget me :some-visible?)
      (mxr/div {:className "toggle-all-container"}
        (mxr/input {:className "toggle-all" :type "checkbox"
                    :defaultChecked (every? :completed? (mx/mget (fmu :main) :visible-todos))
                    :onChange #(toggle-all! me (.-checked (.-target %)))})
        (mxr/label {:className "toggle-all-label"} "Toggle All Input")))
    (mxr/ul {:className "todo-list"}
      {:kid-values (mx/cF (mx/mget (fmu :main) :visible-todos))
       :kid-key #(mx/mget % :key)
       :kid-factory (fn [_ {:keys [id completed?] :as todo}]
                      (mxr/li (if completed? {:className "completed"} {})
                        {:key id}
                        (Item todo)))}
      (mx/kid-values-kids me _cache))))

(defn footer []
  (mxr/footer
    {:className "footer"}
    {:name :footer
     :active-count (mx/cF (count (filter :completed? (todo-list me))))}
    (mxr/span {:className "todo-count"}
      (let [c (mx/mget (fmu :footer) :active-count)]
        (str c " " (if (<= c 1) "item" "items") " left")))
    (mxr/ul {:className "filters"}
      (for [[href txt]  [["#/" "All"] ["#/active" "Active"] ["#/completed" "Completed"]]
            :let [selected? (= href (router-hash me))
                  prop (merge (when selected? {:className "selected"}) {:href href})]]
        (mxr/li {} (mxr/a prop txt))))
    (mxr/button {:className "clear-completed"
                 :disabled (mx/mget me :disabled?)
                 :onClick #(remove-completed! me)}
      {:disabled? (mx/cF (= (mx/cF (count (todo-list me)))
                            (mx/mget (fmu :footer) :active-count)))}
      "Clear completed")))

(defn TodoMVC []
  (mx/make
    :todomvc
    :rx-dom
    (mx/cFonce
      (mxr/div {:className "todoapp"}
        (make-todo me)
        (make-router me)

        (Header)
        (Main)
        (footer)))))
