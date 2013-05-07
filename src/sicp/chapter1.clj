(ns sicp.chapter1
  "Short package description."
  (:use [criterium.core]
        [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(use 'criterium.core)
;;1.11
(defn f
  "recursive verion"
  [n]
  (if (< n 3)
    n
    (+ (f (dec n))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(defn f
  "iterative version"
  [n]
  (letfn [(iter [a b c count]
            (if (< count 3)
              a
              (recur (+ a (* 2 b) (* 3 c))
                     a
                     b
                     (dec count))))]
    (iter 2 1 0 n)))

;;1.12
(defn pascal
  "pascal triangle idomatic clojure lazy seq way"
  [row col]
  (letfn [(lazypascal []
            (lazy-cat [[1]] (map #(map + (cons 0 %) (concat % [0]))
                                 (lazypascal))))]
    (nth (nth (lazypascal) row) col)))

;;1.14
(defn count-change
  "count the number of ways for changes for amount (cents)"
  [amount]
  (letfn [(cc [amount kinds-of-coins]
            (cond
             (= amount 0) 1
             (or (< amount 0) (= kinds-of-coins 0)) 0
             :else (+ (cc amount
                          (- kinds-of-coins 1))
                      (cc (- amount
                             (first-denomination kinds-of-coins))
                          kinds-of-coins))))
          (first-denomination [kinds-of-coins]
            (condp = kinds-of-coins
              1 50
              2 25
              3 10
              4 5
              5 1))]
    (cc amount 5)))
;; 1.15
(defn sine
  "calculate sine value approximately the smaller the precision is
  ie. 0.01, the accurate the result is"
  [angle precision]
  (letfn [(cube [x] (* x x x))
          (p [x] (- (* 3 x) (* 4 (cube x))))]
    (if (< (Math/abs angle) precision)
      angle
      (p (sine (/ angle 3.0) precision)))))

;;1.16
(defn expo
  "calculate the exponent"
  [b n]
  (letfn [(iter [a b n]
            (cond (= n 0) a
                  (even? n) (recur a (* b b) (/ n 2))
                  :else (recur (* a b) b (dec n))))]
    (iter 1 b n)))

;;1.19
(defn fib
  "naive version which generate infinite lazy fib seq"
  ([] (lazy-cat [0 1] (fib 0 1)))
  ([a b] (lazy-seq (cons (+ a b) (fib b (+ a b))))))

(defn fib
  "fib with matrix version which can generate infi seq when combined
  with map"
  [n]
  (let [T (matrix [[1 1] [1 0]])]
    (first (first (* (expo (matrix [[1 1] [1N 0]]) n) (matrix [[1 0]]))))))

(defn fib
  "even faster implementation of fib can compute up to 1475th fib number"
  [n]
  (let [phi (/ (inc (Math/sqrt 5)) 2)]
    (Math/floor (+ (/ 1 2) (/ (expo phi n) (Math/sqrt 5))))))



;;1.20
