(ns fourclojure.core)

(defn last-element
  "Implementation of last"
  [param]
  (if (empty? param) 
    nil 
    (if (= (rest param) '())
      (first param)
      (last-element (rest param)))))

(= (last-element [1 2 3 4 5]) 5)

(defn count-a-sequence
  "Implementation of count"
  [param]
  (if (= '() param)
    0
    (+ 1 (count-a-sequence (rest param)))))

(count-a-sequence "This is a string")

(defn reverse-a-sequence
  "implementation of reverse/rseq"
  [param]
  (if (empty? param) 
    '()
    (into (list (first param)) (rest param))))

(reverse-a-sequence [])

(defn fibonacci-sequence
  "Returns the nth fibonacci number as a sequence containing all
  fibonacci numbers < n"
  [n] 
  (if (= n 1)
    '(1)
    (if (= n 2)
      '(1 1)
      (seq (conj (vec (fibonacci-sequence (- n 1)))
            (+ (last (fibonacci-sequence (- n 1))) (last (fibonacci-sequence (- n 2)))))))))

(fibonacci-sequence 5)

(defn maximum-value
  "Implementation of max"
  [head & tail]
  (if (empty? tail)
    head
    (if (>= head (first tail))
      (apply maximum-value (conj (rest tail) head))
      (apply maximum-value tail))))


(maximum-value 1 2 3 4 5 6)

(defn palindrome-detecor
  "Determines if a collection is a palindrome"
  [param]
  (if (= (seq param) (reverse param))
    true
    false))

(palindrome-detecor "racecar")

(defn nth-element
  "Implementation of nth"
  [param index]
  (if (= 0 index)
    (first param)
    (nth-element (rest param) (dec index))))

(nth-element [1 2 3 4] 3)

(defn my-range
  "Implementation of range"
  [lower upper]
  (if (= lower (- upper 1))
    (list lower)
    (conj (my-range (inc lower) upper) lower)))

(my-range -4 4)

(defn interleave-seq
  "Implementation of interleave"
  [seqone seqtwo]
   (if (empty? (rest seqone))
    (list (first seqone) (first seqtwo)) 
    (if (empty? (rest seqtwo))
      (list (first seqone) (first seqtwo))
      (conj (interleave-seq (rest seqone) (rest seqtwo)) (first seqtwo) (first seqone)))))

(interleave-seq [1 2 5] [3 4 7])

(defn duplicate
  "Duplicates all values in a sequence in order"
  [param]
  (if (empty? (rest param))
    (list (first param) (first param))
    (conj (duplicate (rest param)) (first param) (first param))))

(duplicate [1 2 3])
