(ns assignment2.core)

(require '[clojure.string :as string])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn divisors
  [n]
  (filter (fn [x]
            (= 0 (mod n x)))
          (rest (range (inc n)))))

(divisors 7)
(divisors 24)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn abundance
  [n]
  (- (reduce + (butlast (divisors n))) n))

(abundance 12)

(defn abundant?
        [n]
        (> (abundance n) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(filter abundant? (range 300))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 4
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pattern-count
  [text pattern]
  (count 
    (filter #(= (seq pattern) %) 
            (partition (count pattern) 1 
                       text))))

(pattern-count "abababa" "aba")
(pattern-count "aaaaa" "aa")
(pattern-count "Abcde" "abc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn freq-map
  [string length]
  (map #(vector % (pattern-count string %)) 
       (set (partition length 1 string))))

(freq-map "abacaba" 4)
(freq-map "TCGAAGCTAGACGCTAGTAGCTAGTGTGCA" 4)

(defn most-frequent-word
  [string length]
  (let [freqmap (freq-map string length)
        maxocc (reduce max (map #(second %) freqmap))]
    (map #(apply str (first %)) 
         (filter #(= maxocc (second %)) freqmap))))

(most-frequent-word "TCGAAGCTAGACGCTAGTAGCTAGTGTGCA" 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 6
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def text "CGGACTCGACAGATGTGAAGAAATGTGAAGACTGAGTGAAGAGAAGAGGAAACACGACACGACATTGCGACATAATGTACGAATGTAATGTGCCTATGGC")

(defn find-clumps
  [string k L t]
  (let [possible-clumps (partition L 1 string)]
   (map #(apply str (first %)) (filter (fn 
              [[in _]]
              (> 
                (count (filter #(>= (pattern-count % in) t) possible-clumps)) 
                0)) 
      (filter #(>= (second %) t) (freq-map string k))))
   
    ))

(find-clumps text 5 75 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # 7
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def path "/home/ted/school/CS696/assignments/assignment2/src/assignment2/weather.dat" )

(defn- find-spread
  [input]
  (- (read-string (first (re-seq #"[\d.]+" (second input)))) 
     (read-string (first (re-seq #"[\d.]+" (nth input 2))))))

(defn maximum-spread
  [path]
  (let [data (map #(string/split % #"\t")
               (subvec (string/split (slurp path) #"\n") 2))]
    
    (read-string 
      (ffirst 
          (filter #(= (reduce max (map find-spread data)) (find-spread %)) 
                  data)))))

(maximum-spread path)
   
   
   
