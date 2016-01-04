(ns assignment3.core
  (:require [quil.core :refer :all])
  (:import [java.awt.event KeyEvent]))

(require '[clojure.string :as str])

(def current-text-size (atom 12))

(def params {:big-text-size 30
	:background-color 75
	:foreground-color 200})

(def command-list 
  (map #(str/split % #",") 
    (str/split 
      (slurp (str (System/getProperty "user.dir") "/src/assignment3/pic.csv")) 
    #"\n")))

(defn pos-watch
  [k r old-state new-state]
  (when @(state :pen)
   (swap! (state :draw-list)
          (fn [xs]
           (conj xs [(old-state 0) (old-state 1) 
                     (new-state 0) (new-state 1)])))))

(defn setup []
  (smooth)
  (no-stroke)
  (no-loop)
  (set-state! 
    :pos (atom [400 400])
    :bearing (atom 0)
    :pen (atom true) 
    :message (atom "Initial state")
    :draw-list (atom '())
    :idx (atom 0)
    :cmd-list (atom command-list))
  (add-watch (state :pos) :move pos-watch))

(defn pen-up!
  []
  (when (= @(state :pen) true)
    (reset! (state :message) (str "Last: pen-up"))
    (reset! (state :pen) false)))

(defn pen-down!
  []
  (when (= @(state :pen) false)
    (reset! (state :message) (str "Last: pen-down"))
    (reset! (state :pen) true)))

(defn move!
  [px]
  (reset! (state :message) (str "Last: move " px))
  (swap!  (state :pos) 
         (fn [pos]
           [(+ (first  pos) (* px (cos @(state :bearing))))
            (+ (second pos) (* px (sin @(state :bearing))))])))

(defn turn!
  [degs]
  (reset! (state :message) (str "Last: turn " degs))
  (swap! (state :bearing) 
         (fn [bearing] 
           (+ bearing (* DEG-TO-RAD degs)))))

(defn do-step
  []
  (when (neg? @(state :idx)) (reset! (state :idx) 0))
  (if (<= @(state :idx) (dec (count @(state :cmd-list))))
    (let [cmd (nth @(state :cmd-list) @(state :idx))
          fun (first cmd)]

      (cond 
        (= fun "pen-up") (pen-up!)
        (= fun "pen-down") (pen-down!)
        (= fun "move") (move! (read-string (second cmd)))
        (= fun "turn") (turn! (read-string (second cmd)))
        :else (println "Somethings wrong..."))
      (swap! (state :idx) inc))
    (no-loop)))

(defn undo-step
  []
  (swap! (state :idx) dec)
  (when (> @(state :idx) (dec (count @(state :cmd-list)))) 
    (reset! (state :idx) (dec (count @(state :cmd-list)))))
  (when (>= @(state :idx) 0)
    (let [cmd (nth @(state :cmd-list) @(state :idx))
          fun (first cmd)]
      (cond 
        (= fun "pen-up") (pen-down!)
        (= fun "pen-down") (pen-up!)
        (= fun "move") (do (remove-watch (state :pos) :move) 
                         (move! (- (read-string (second cmd))))
                         (add-watch (state :pos) :move pos-watch)
                          (when @(state :pen)
                            (swap! (state :draw-list) pop))
                            (redraw))
        (= fun "turn") (turn! (- (read-string (second cmd)))))
      (if (= @(state :idx) 0) 
        (reset! (state :message) (str "Initial state"))
        (let [last-command (nth @(state :cmd-list) (dec @(state :idx)))]
          (reset! (state :message) 
                  (str "Last: " (first last-command) " " (or (second last-command) ""))))))))

(defn run
  []
  (loop [i @(state :idx)]
    (do-step)
    (if (>= @(state :idx) (count @(state :cmd-list)))
      (redraw)
      (recur i))))

(defn draw
  []
  (background-float (params :background-color))
  (stroke-weight 10)
  (stroke-int 10)
  (fill (params :foreground-color))
  (text-size @current-text-size)
  (text @(state :message) 20 60)
  (doseq [coords @(state :draw-list)]
    (apply line coords)))

(defn key-press []
  (let [raw (raw-key)
        code (key-code)
        pressed (if (= processing.core.PConstants/CODED (int raw)) code raw)]
    
    (cond 
      (= pressed 39) (do-step)
      (= pressed 37) (undo-step)
      (= pressed \r) (run)
      (= pressed \p) (println @(state :draw-list) "--" @(state :idx))
      :else (println "Caught: " pressed))
    )
    (redraw)) 

(defsketch turtle 
  :title "Turtle Graphics Demo"
  :size [800 800]
  :setup setup
  :draw draw
  :key-pressed key-press)

(defn -main [& args])
