(ns assignment4.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defn new-board [n]
  (vec (repeat n (vec (repeat n 0)))))

(def board-size 3)

(def win-length 3)

(defonce app-state (atom {:text "Welcome to tic tac toe"
                          :board (new-board 3)}))

(defn computer-move []
  (let [board (:board @app-state)
        remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) 0)]
                          [i j])
        move (rand-nth remaining-spots)
        path (into [:board] (reverse move))] 
  (swap! app-state assoc-in path 2)))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
          (for [i (range n)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn full? [board]
  (every? #{1 2} (apply concat board)))

(defn win? [owner board n]
  (some true?
          (for [i (range board-size)
                j (range board-size)
                dir [[1 0] [0 1] [1 1] [1 -1]]]
           (straight owner board [i j] dir n))))

(defn new-game-click [e]
       (swap! app-state assoc
              :board (new-board board-size)))

(defn blank [i j]   
  [:rect {:width 0.9
          :height 0.9
          :fill "grey"
          :x i
          :y j
          :on-click
          (fn rect-click [e]
            (prn "You clicked me!" i j)
            (prn (swap! app-state update-in [:board j i] inc))
           (when (not (full? (:board @app-state))) (computer-move))
            (when (win? 1 (:board @app-state) win-length)
              (js/alert "You win!")
              (new-game-click (:board @app-state)))
            (when (win? 2 (:board @app-state) win-length)
              (js/alert "The computer won...")
              (new-game-click (:board @app-state))))}])

(defn circle [i j]
  [:circle 
   {:r 0.35  
   :fill "none"
   :stroke-width 0.1
   :stroke "green"
   :cx (+ 0.5 i)
   :cy (+ 0.5 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
       :stroke-width 0.35
       :stroke-linecap "round"
       :transform
       (str "translate (" (+ 0.5 i) "," (+ 0.5 j) ")" 
            "scale(0.3)")}
   [:line {:x1 -1 :y1 -1 :x2 1 :y2 1}]
   [:line {:x1 1 :y1 -1 :x2 -1 :y2 1}]])

(defn tictactoe []
 [:center
   [:h1 (:text @app-state)] 
   (into 
    [:svg
      {:view-box "0 0 3 3"
       :width 500
       :height 500}]
      (for [i (range (count (:board @app-state)))
            j (range (count (:board @app-state)))]
       (case (get-in @app-state [:board j i])
         0 [blank i j]
         1 [circle i j]
         2 [cross i j])))
   [:button
    {:on-click new-game-click}
    "New Game"]])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))


(defn on-js-reload [])
