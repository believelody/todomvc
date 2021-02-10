(ns todomvc.app.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [cljs.pprint :refer [pprint]]))

;; --- App State ---

(def initial-todos {1 {:id 1, :title "Do laundry", :done false}
                    3 {:id 3, :title "Buy groceries", :done false}
                    2 {:id 2, :title "Wash dishes", :done true}})

(def initial-todos-sorted (into (sorted-map) initial-todos))

(defonce todos (r/atom initial-todos-sorted))

(defonce counter (r/atom (count initial-todos)))

;; --- Watch the State ---

(add-watch todos :todos
           (fn [key _atom _old-state new-state]
             (println "---" key "atom changed ---")
             (pprint new-state)))

;; --- Utilities ---

(defn add-todo [text]
  (let [id (swap! counter inc)
        new-todo {:id id, :title text, :done false}]
    (swap! todos assoc id new-todo)))

;; --- Views ---

(defn todo-input []
  (let [input-text (r/atom "")
        update-text #(reset! input-text %)
        reset-text #(reset! input-text "")
        save #(do
                (add-todo @input-text)
                (reset-text))
        key-pressed #(case %
                       "Enter" (save)
                       "Esc" (reset-text)
                       "Escape" (reset-text)
                       nil)]
    (fn []
      [:input {:class "new-todo"
               :placeholder "Todo input"
               :type "text"
               :value @input-text
               :on-blur save
               :on-key-down #(key-pressed (.. % -key))
               :on-change #(update-text (.. % -target -value))}])))

(defn todo-item [{:keys [title]}]
  [:li
   [:div.view
    [:label title]]])

(defn task-list []
  (let [items (vals @todos)]
    [:section.main
     [:ul.todo-list
      (for [todo items]
        ^{:key (:id todo)}[todo-item todo])]]))

(defn footer-controls []
  [:footer.footer
   [:div "Footer controls"]])

(defn task-entry []
  [:header.header
   [:h1 "todos"
    [todo-input]]])

(defn todo-app []
  [:div
   [:section.todoapp
    [task-entry]
    [:div
     [task-list]
     [footer-controls]]]
   [:footer.info
    [:p "Footer info"]]])

;; --- Render ---

(defn render []
  (rdom/render [todo-app] (.getElementById js/document "root")))

(defn ^:export main []
  (render))

(defn ^:dev/after-load reload! []
  (render))
