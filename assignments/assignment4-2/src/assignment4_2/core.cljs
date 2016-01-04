(ns assignment4-2.core
  (:require [reagent.core :as r :refer [atom]]
            [cljs.core.async :as a]
            [schema.core :as s :include-macros true]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (r/atom {
                            :mode :line 
                            :drawables '()
                            :curr []}))

(add-watch app-state ::validator (fn [_ _ ostate nstate]
                                   (s/validate s/Keyword (nstate :mode))
                                   (s/validate [s/Num] (nstate :curr))))

(def preview (r/atom '()))
(def history (r/atom (list @app-state)))

(def mode (r/cursor app-state [:mode]))
(def drawables (r/cursor app-state [:drawables]))
(def curr (r/cursor app-state [:curr]))

(def canvas-items (r/atom @drawables))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(def canvas-coords {:x 10 :y 40})

(defn points->hiccup-line 
  [[x1 y1 x2 y2]]
  (apply (partial s/validate s/Num) [x1 y1 x2 y2])
  [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2}])

(defn points->hiccup-circle 
  [[x1 y1 x2 y2]]
  (apply (partial s/validate s/Num) [x1 y1 x2 y2])
  [:circle {:cx x1 :cy y1 :r (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))) :fill "red"}])

(defn points->hiccup-rect 
  [[xa ya xb yb]]
  (apply (partial s/validate s/Num) [xa ya xb yb])
  (let [x1 (min xa xb)
        x2 (max xa xb)
        y1 (min ya yb)
        y2 (max ya yb)]
    [:rect {:x x1 :y y1 :width (Math/abs (- x1 x2))  :height (Math/abs (- y1 y2))}]))

(defn on-click [e]
 (let [ x (- (.-clientX e) (canvas-coords :x)) 
        y (- (.-clientY e) (canvas-coords :y))]
   (if (empty? @curr) 
     (reset! curr [x y])
     (let [points [(first @curr) (second @curr) x y]]                            
                          (cond 
                            (= :line @mode) (swap! drawables conj (points->hiccup-line points)) 
                            (= :circle @mode) (swap! drawables conj (points->hiccup-circle points))
                            (= :rect @mode) (swap! drawables conj (points->hiccup-rect points)))
                            (swap! curr empty)
                            (swap! history conj @app-state)
                            (reset! canvas-items @drawables)))))

(defn mode-select [k]
  (reset! mode k)
  (swap! history conj @app-state))

(defn move-mouse [e]
  (when (not (empty? @curr))
    (let [points [(first @curr) 
                 (second @curr) 
                 (- (.-clientX e) (canvas-coords :x))
                 (- (.-clientY e) (canvas-coords :y))]]                            
      (cond 
        (= :line @mode) (reset! canvas-items (conj @drawables (points->hiccup-line points)))
        (= :circle @mode) (reset! canvas-items (conj @drawables (points->hiccup-circle points)))
        (= :rect @mode)(reset! canvas-items (conj @drawables (points->hiccup-rect points)))))))

(defn undo []
  (let [undos @history] 
    (when-let [old (first (rest undos))]
      (reset! app-state old)
      (reset! history (rest undos))
      (reset! canvas-items @drawables))))

(defn clear []
  (reset! app-state {
                      :mode (@app-state :mode) 
                      :drawables '()
                      :curr []})
  (reset! history (list @app-state))
  (reset! canvas-items @drawables))

(defn main []
 [:div
    [:button {:on-click #(mode-select :line)} "Line"]
    [:button {:on-click #(mode-select :circle)} "Circle"]
    [:button {:on-click #(mode-select :rect)} "Rectangle"]
    [:button {:on-click undo} "Undo"]
    [:button {:on-click clear} "Clear"]    
    [:div
      {:on-click on-click
       :on-mouse-move move-mouse} 
      [:svg {:width 600 :height 600 :stroke "black"
            :style {:position :fixed :top (canvas-coords :y) :left (canvas-coords :x) :border "red solid 1px"}}
       @canvas-items
       ]]
    ])

(r/render-component [main]
                          (. js/document (getElementById "app")))


