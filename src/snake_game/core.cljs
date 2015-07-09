(ns ^:figwheel-always snake-game.core
      (:require-macros [cljs.core.async.macros :refer [go go-loop]])

    (:require
     [reagent.core :as reagent :refer [atom]]
     [cljs.core.async :as async :refer [chan put! <!]]

     [goog.events :as events]
     ))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload
(def grid-x 20)
(def grid-y 20)

(def directiron-delta {
                       :up [-1 0]
                       :down [1 0]
                       :right [0 1]
                       :left [0 -1]
                       :key-not-found [1 1]
                       })

(defonce snake-full-list (atom
                          {:direction :up
                           :body '([2 2]
                                   [2 3]
                                   [2 4]
                                   [2 5]
                                   [2 6]
                                   [2 7]
                                   [2 8]
                                   [2 9])
                           }))

(defn- mod-point [point]
  (vec (map mod point [grid-x grid-y])))

(defn- next-head [head direction]
  (let [delta (direction directiron-delta)
        next-head (map + head delta)]
    (mod-point next-head)))

(defn- next-tail [snake-list]
  (let [tail-pre (last (butlast snake-list))]
    (mod-point tail-pre)))

(defn- move [snake]
  (let [snake-full-list (:body @snake)
        head (first snake-full-list)
        head-next (next-head head (:direction @snake))
        tail (last snake-full-list)
        tail-next (next-tail snake-full-list)]
    (swap! snake update-in [:body] #(cons head-next %))
    (swap! snake update-in [:body] #(concat (butlast %) (list tail-next)))
    (if (= (last (:body @snake)) (last (butlast (:body @snake))))
      (swap! snake update-in [:body] butlast))))

(defn cell [x y]
  (let [cord {:x x :y y}]
    [:div.cell
     (if (some #(= [x y] %) (:body @snake-full-list))
       {:style {:background-color "green"}})
     ]))

(defn row [row cols]
  "render a row with n divs"
  [:div.row
   {:key (str "row-" row)}
   (for [y (range cols)]
     (let [cord (atom {:x row :y y})]
       ^{:key (str "cell-" row y)}
       [cell row y]))])

(defn snake-world []
  [:div
   [:h1 "Snake Game"]
   (for [r (range grid-x)]
     (row r grid-y))
   [:div {:style {:clear "both"}}]
   [:h1 (pr-str "")]
   ])

(defn render []
  (reagent/render-component [snake-world]
                            (. js/document (getElementById "app"))))

(add-watch snake-full-list :render render)

(render)


(def keycodes
  {
   37 :left
   38 :up
   39 :right
   40 :down
   })

(defn event->key [ev]
  "Transform the js event object to the command key"
  (get keycodes (.-keyCode ev) :key-not-found))

(defn event-chan []
  (let [ev-chan (chan)]
    (events/listen (.-body js/document)
                   (.-KEYDOWN events/EventType)
                   #(do
                      (put! ev-chan (event->key %))))
    ev-chan))

(def ev-chan (event-chan))

(go
  (loop []
    (let [key (<! ev-chan)]
      (swap! snake-full-list update-in [:direction] #(let [] key)))
    (recur)))

(js/setInterval #(move snake-full-list) 500)

;; (swap! snake-full-list update-in [:direction] #(let [] :down))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
