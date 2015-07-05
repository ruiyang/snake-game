(ns ^:figwheel-always snake-game.core
    (:require
              [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload
(def grid-x 20)
(def grid-y 20)

(def snake (atom [[2 2]
                  [4 2]
                  [4 8]]))

(defn- is-vertical [cord1 cord2]
  (= (cord1 1)
     (cord2 1)))

(defn- full-points [start end]
  (let [start-x (start 0)
        start-y (start 1)
        end-x (end 0)
        end-y (end 1)
        delta-x (if (> start-x end-x) -1 1)
        delta-y (if (> start-y end-y) -1 1)]
    (if (is-vertical start end)
      (map #(list % start-y) (range start-x (+ end-x delta-x) delta-x))
      (map #(list start-x %) (range start-y (+ end-y delta-y) delta-y)))))

(defn- snake-cord-list []
  (distinct
   (reduce concat
           (map full-points
                (butlast @snake)
                (rest @snake)))))

(defn- next-point [point start end]
  (let [delta (map - start end)
        next-point-vec (map + point delta)]
    next-point-vec
    ))

(defn- move [snake]
  (let [snake-full-list (snake-cord-list)
        head (first snake-full-list)
        head-next (first (rest snake-full-list))
        head-next-state (next-point head head head-next)
        tail (last snake-full-list)
        tail-next (last (butlast snake-full-list))
        tail-next-state (next-point tail tail-next tail)]
    (swap! snake assoc 0 head-next-state)
    (swap! snake assoc (- (count @snake) 1) tail-next-state)
    (if (= (last @snake) (last (butlast @snake)))
      (swap! snake pop))))

(defn cell [x y]
  (let [cord {:x x :y y}]
    [:div.cell
     (if (some #(= [x y] %) (snake-cord-list))
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

(defn snake-grid-world []
  [:div
   [:h1 "Snake Game"]
   (for [r (range grid-x)]
     (row r grid-y))
   [:div {:style {:clear "both"}}]
   [:h1 (pr-str "")]
   ])

(defn render []
  (reagent/render-component [snake-grid-world]
                            (. js/document (getElementById "app"))))

(add-watch snake :render render)

(render)

(js/setInterval #(move snake) 1000)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
