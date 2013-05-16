(ns sicp.chapter2
  "Short package description."
  (:refer-clojure :exclude [== + * / -])
  (:require [clojure.core :as core]
            [criterium.core]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :refer :all ]
            [alembic.still :as deps]
            [clojure.math.combinatorics :as combo]
;            [clojure.math.numeric-tower :exclude [sqrt] :refer :all]
            [clojure.tools.trace :refer :all]
            [incanter.infix :refer :all]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]))


;;ex 2.1
(defn make-rat
  "make a rational number"
  [n d]
  (/ n d))

;;ex 2.2
(defn make-point
  "make a poinr"
  [a b]
  [a b])

(defn make-segment
  "make a line"
  [a b]
  [a b])

(defn start-segment
  "starting point"
  [line]
  (first line))

(defn end-segment
  "starting point"
  [line]
  (last line))

(defn x-point
  "x cord of poinr"
  [pt]
  (first pt))

(defn y-point
  "x cord of poinr"
  [pt]
  (last pt))

(defn mid-point-segment
  "find the mid point"
  [line]
  (make-point (/ (+ (x-point (start-segment line))
                    (x-point (end-segment line))) 2)
              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line))) 2)))

;;ex 2.30 & 2.31
(defn square-tree
  "square each element in a tree"
  [tree f]
  (map #(if (coll? %)
          (square-tree %)
          (f %))
       tree))

;;ex 2.32
(defn subsets [s]
  (if (seq s)
    (let [others (subsets (rest s))]
      (concat others (map #(cons (first s) %)
                          others)))
    '(())))

;;ex 2.34
(defn horner-eval
  "evaluate a polynomial sequence ie. to eval X^2 + X + 1 where X = 2,
  x = 2, eval = [1 1 1]"
  [x coeff]
  (reduce (fn [curr higher]
            (+ higher (* curr x)))
          coeff))
(defn dot
  "dot product"
  [v w]
  (apply + (map * v w)))

(defn m-*
  "matrix mult vector"
  [m v]
  (for [w m]
    (dot w v)))
(defn transpose
  "transpose a matrix"
  [m]
  (apply map vector m))

(defn m-*-m
  "matrix mult matrix"
  [m1 m2]
  (for [w m1]
    (for [v (transpose m2)]
      (dot w v))))

(defn p-matrix
  "print out a matrix"
  [m]
  (doseq [v m]
    (apply println v)))
;;2.39
(reduce #(cons %2 %1) [] [1 2 3])
;;2.40
(defn unique-pair
  "create some unique pairs"
  [n]
  (for [i (range n)
        j (range n)
        :when (< 0 i j n)]
    [i j]))

(defn sum-three
  "lalala"
  [n s]
  (for [i (range n)
        j (range n)
        k (range n)
        :when (and (= s (+ i j k))
                   (< k j i n))]
    [i j k]))

(defn sum-three [n s]
  (for [i (range n)
        j (range n)
        k (range n)
        :when (and (= s (+ i j k))
                   (< 1 k j i n))]
    [i j k]))

(defn sum-c [c n s]
  ;; go would return all tuples with c elements in range n-b which sums up to s
  (letfn [(go [c n s b]
            (if (zero? c)
              [[]]
              (for [i (range b n)
                    is (go (dec c) n (- s i) (inc i))
                    :when (== s (apply + i is))]
                (conj is i))))]
    (go c n s 1)))

(defn sum-c-opt [c n s]
  (let [m (max 0 (- s (* (dec c) (dec n))))]
    (if (>= m n)
      (letfn [(go [c s t]
                (if (zero? c)
                  (list t)
                  (mapcat #(go (dec c) (- s %) (conj t %))
                          (range (max (inc (peek t))
                                      (- s (* (dec c) (dec n))))
                                 (min n (inc s))))))]
        (mapcat #(go (dec c) (- s %) (list %)) (range m n))))))

(defn sum-three [n s]
  (mapcat (fn [i]
            (mapcat (fn [j]
                      (filter (fn [[i j k]]
                                (and (= s (+ i j k))
                                     (< 1 k j i n)))
                              (map (fn [k] [i j k]) (range n))))
                    (range n)))
          (range n)))
(defn perm
  "asdl;k"
  [xs]
  (if (seq xs)
    (mapcat (fn [x] (map (fn [y] (cons x y))
                         (perm (remove #{x} xs))))
            xs)
    '(())))

;;2.42
(defn safe?
  "check if the queen in kth column is safe"
  [k board]
  (let [r (first (first (filter #(= k (second %)) board)))
        board (remove #(= k (second %)) board)]
    (and (every? #(not= r (first %)) board)
         (every? #(not= (+ r k) (+ (first %) (second %))) board)
         (every? #(not= (- r k) (- (first %) (second %))) board))))

(defn queens
  "place n queens in board with size rxn"
  [[r n]]
  (letfn [(queen-cols [k]
            (if (== k 0)
              [[]]
              (filter ;; #(and % true)
               (fn [pos] (safe? k pos))
               (mapcat
                (fn [rest-of-queens]
                  (map (fn [new-row]
                         (conj rest-of-queens [new-row k]))
                       (range 1 (inc n))))
                (queen-cols (- k 1))))))]
    (queen-cols n)))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;ex2.56
(defn do-sum [a b & xs]
  (if (< 0 (count xs))
    (apply list '+ (do-sum a b) xs)
    (cond (and (number? a) (number? b)) (+ a b)
          (= a 0) b
          (= b 0) a
          :else (list '+ a b))))

(defn do-product [a b & xs]
  (if (< 0 (count xs))
    (apply list '* (do-product a b) xs)
    (cond (and (number? a) (number? b)) (* a b)
          (or (= a 0) (= b 0)) 0
          (= a 1) b
          (= b 1) a
          :else ( list '* a b))))

(defn addend [exp] (second exp))
(defn augend [exp]
  (let [end (drop 2 exp)]
    (if (= 1 (count end))
      (first end)
      (apply do-sum end))))

(defn multiplier [exp] (second exp))
(defn multiplicand [exp]
  (let [end (drop 2 exp)]
    (if (= 1 (count end))
      (first end)
      (apply do-product end))))

(defn do-expo
  "expo"
  [a b]
  (cond (= 1 b) a
        (= 0 b) 1
        :else (list \^ a b)))

(defn base [exp] (second exp))
(defn exponent [exp] (last exp))

(defn deriv
  "derivative of exp and var"
  [exp var]
  {:pre [(symbol? var)]}
  (cond (number? exp) 0
        (symbol? exp) (if (= exp var) 1 0)
        (= '+ (first exp)) (do-sum (deriv (addend exp) var)
                                   (deriv (augend exp) var))
        (= '* (first exp)) (do-sum
                            (do-product (multiplier exp)
                                        (deriv (multiplicand exp) var))
                            (do-product (deriv (multiplier exp) var)
                                        (multiplicand exp)))
        (= \^ (first exp)) (do-product (deriv (base exp) var)
                                       (exponent exp)
                                       (do-expo (base exp) (dec (exponent exp))))
        :else (println "error occured")))
;;TODO: simplify ther final version
(defn find-hi
  "find the pos of lowest operator"
  [exp]
  (let [pre-table {'+ 0, '- 0, '* 1, '/ 1, \^ 3}
        cmp (fn [a b] (cond (<= (pre-table (first a))
                               (pre-table (first b)))
                            (if (< (second a) (second b)) -1 1)
                            :else -1))
        postion-map (-> (map-indexed #(identity [%2 %1]) exp)
                        reverse
                        flatten
                        ((partial apply hash-map)))]
    (second (first (sort cmp (filter #(pre-table (first %))
                                     postion-map))))))
(defn indices
  "nice function that find the indicie of matching predicate in coll"
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn find-hi
  "doc-string"
  [exp]
  (let [pre-table #{'+ '- '* '/ \^}]
    ;; (second (first
    ;;         (filter #(pre-table (first %))
    ;;                 (map-indexed #(identity [%2 %1]) exp))))
    (first (keep-indexed #(when (pre-table %2) %1) exp))))

(defn in2pre
  "infix to prefix"
  [exp]
  (cond (not (coll? exp)) exp
        (= 1 (count exp)) (in2pre (first exp))
        :else (let [hi (find-hi exp)]
                (if  (nil? hi)
                  exp
                  (let [[hd [op & tl]] (split-at hi exp)]
                    (list op (in2pre hd) (in2pre tl)))))))

(def text-exp '(1 + 2 + 3 * 5 * 4))

(defn count-string
  "split the string than count "
  [re s]
  (count (re-seq re s)))

;;ex
