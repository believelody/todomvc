(ns todomvc.app.core
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            ;; [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [secretary.core :as secretary :refer-macros [defroute]]
            [goog.events :as gevents]
            [goog.history.EventType :as EventType])
  (:import goog.history.Html5History))

;; --- App State ---

;; (def initial-todos {1 {:id 1, :title "Do laundry", :done false}
;;                     3 {:id 3, :title "Buy groceries", :done false}
;;                     2 {:id 2, :title "Wash dishes", :done true}})

;; (def initial-todos-sorted (into (sorted-map) initial-todos))

(defonce db (r/atom {:todos (sorted-map)
                     :showing :all}))

(defonce todos (r/cursor db [:todos]))

(defonce showing (r/cursor db [:showing]))

;; (defonce counter (r/atom 0))

;; --- Local Storage ---

(def local-store-key "todo-app")

(defn todos->local-store []
  (.setItem js/localStorage local-store-key (str @todos)))
  ;; (.setItem js/localStorage local-store-key (str todos)))

(defn local-store->todos []
  (let [edn-map-todos (.getItem js/localStorage local-store-key)
        unsorted-todos (some->> edn-map-todos reader/read-string)
        sorted-todos (into (sorted-map) unsorted-todos)]
    (reset! todos sorted-todos)))
    ;; (reset! todos sorted-todos)))

;; --- Watch the State ---

(add-watch db :db
           (fn [_key _atom old-state new-state]
             (when (not= (:todos new-state) (:todos old-state))
               (todos->local-store))
             #_(println "---" key "atom changed ---")
             #_(pprint new-state)))

;; --- Utilities ---

(defn allocate-next-id [todos]
  ((fnil inc 0) (last (keys todos))))

(defn set-showing [kw]
  (reset! showing kw))

(defn add-todo [text]
  (let [id (allocate-next-id @todos)
        new-todo {:id id, :title text, :done false}]
    (swap! todos assoc id new-todo)))

(defn save-todo [id title]
  (swap! todos assoc-in [id :title] title))

(defn delete-todo [id]
  (swap! todos dissoc id))

(defn toggle-done [id]
  (swap! todos update-in [id :done] not))

(defn mmap [m f g]
  (->> m
       (f g)
       (into (empty m))))

(defn complete-all-toggle [is-complete?]
  (let [g #(assoc-in % [1 :done] is-complete?)]
    (swap! todos mmap map g)))

(defn clear-completed []
  (let [g #(get-in % [1 :done])]
    (swap! todos mmap remove g)))

;; --- Hash-based Routing ---

(defn hook-browser-navigation! []
  (doto (Html5History.)
    (gevents/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.. ^js event -token))))
    (.setEnabled true)))

(defn app-routes []
  (secretary/set-config! :prefix "#")
  (defroute "/" [] (set-showing :all))
  (defroute "/:filter" [filter] (set-showing (keyword filter)))
  (hook-browser-navigation!))

;; --- Init App with Sample Data ---

#_(defonce init (do
                (add-todo "Do laundry")
                (add-todo "Wash dishes")
                (add-todo "Buy groceries")))

;; --- Views ---

(defn todo-input [{:keys [class placeholder title on-save on-stop]}]
  (let [input-text (r/atom title)
        update-text #(reset! input-text %)
        reset-text #(do (reset! input-text "")
                        (when on-stop (on-stop)))
        save #(let [trimmed-text (-> @input-text str str/trim)]
                (when-not (empty? trimmed-text)
                  (on-save trimmed-text))
                (reset-text))
        key-pressed #(case %
                       "Enter" (save)
                       "Esc" (reset-text)
                       "Escape" (reset-text)
                       nil)]
    (fn []
      [:input {:class class
               :auto-focus true
               :placeholder placeholder
               :type "text"
               :value @input-text
               :on-blur save
               :on-key-down #(key-pressed (.. % -key))
               :on-change #(update-text (.. % -target -value))}])))

(defn todo-item [_]
  (let [editing (r/atom false)]
    (fn [{:keys [id title done]}]
      [:li {:class (str (when done "completed")
                        (when @editing "editing"))}
       [:div.view
        [:input {:class "toggle"
                 :type "checkbox"
                 :checked done
                 :on-change #(toggle-done id)}]
        [:label {:on-double-click #(reset! editing true)} title]
        [:button.destroy {:on-click #(delete-todo id)}]]
       (when @editing
         [todo-input {:class "edit"
                      :title title
                      :on-save (fn [text] (save-todo id text))
                      :on-stop #(reset! editing false)}])])))

(defn task-list []
  (let [items (vals @todos)
        filter-fn (case @showing
                    :done :done
                    :active (complement :done)
                    :all identity)
        visible-items (filter filter-fn items)
        all-complete? (every? :done items)]
    [:section.main
     [:input  {:id "toggle-all"
               :class "toggle-all"
               :type "checkbox"
               :checked all-complete?
               :on-change #(complete-all-toggle (not all-complete?))}]
     [:label {:for "toggle-all"} "Mark all as complete"]
     [:ul.todo-list
      (for [todo visible-items]
        ^{:key (:id todo)}[todo-item todo])]]))

(defn footer-controls []
  (let [items (vals @todos)
        done-count (count (filter :done items))
        active-count (- (count items) done-count)
        props-for (fn [kw]
                    {:class (when (= kw @showing) "selected")
                     :on-click #(reset! showing kw)
                     :href (str "#/" (name kw))})]
    [:footer.footer
     [:span.todo-count
      [:strong active-count] " " (case active-count
                                   1 "item"
                                   "items") " left"]
     [:ul.filters
      [:li [:a (props-for :all) "All"]]
      [:li [:a (props-for :active) "Active"]]
      [:li [:a (props-for :done) "Completed"]]]
     (when (pos? done-count)
       [:button.clear-completed {:on-click clear-completed} "Clear completed"])]))

(defn task-entry []
  [:header.header
   [:h1 "todos"
    [todo-input {:class "new-todo"
                 :placeholder "What needs to be done ?"
                 :on-save add-todo}]]])

(defn todo-app []
  (fn []
    [:div
     [:section.todoapp
      [task-entry]
      (when (seq @todos)
        [:div
         [task-list]
         [footer-controls]])]
     [:footer.info
      [:p "Double-click to edit a todo"]]]))

;; --- Render ---

(defn render []
  (rdom/render [todo-app] (.getElementById js/document "root")))

(defn ^:export main []
  (local-store->todos)
  (app-routes)
  (render))

(defn ^:dev/after-load reload! []
  (render))
