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
            [clojure.repl :refer :all]
            [clojure.zip :as zip]))

(set! *warn-on-reflection* true)

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


;;ex2.56 & 2.57
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
;;ex 2.58
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

(defn find-hi
  "doc-string"
  [exp]
  (let [pre-table #{'+ '- '* '/ \^}]
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



;;ex 2.59 clojure.set/union
(defn union-set
  "find the union of two sets"
  [s1 s2]
  (cond (not (seq s1)) s2
        (not (seq s2)) s1
        (s1 (first s2)) (recur s1 (rest s2))
        :else (recur (conj s1 (first s2)) (rest s2))))
(defn union-set
  "better version"
  [s1 s2]
  (reduce conj s1 s2))

;;my take of binary tree implementation
(defn make-tree [v] {:v v :l nil :r nil})

(defn insert [tree v]
  (if (nil? tree)
    (make-tree v)
    (case (compare v (tree :v))
      -1 (assoc tree :l (insert (:l tree) v))
      0 tree
      1 (assoc tree :r (insert (:r tree) v)))))

(defn find-min [tree]
  (if (tree :l)
    (recur (tree :l))
    (tree :v)))

(defn rm-node
  "remove node from tree"
  [tree v]
  (if-not (nil? tree)
    (case (compare v (tree :v))
      -1 (assoc tree :l (rm-node (:l tree) v)) ;;insert left if smaller than
      1 (assoc tree :r (rm-node (:r tree) v))  ;;insert right if greater than
      0 (if-not (tree :r)                      ;;if tree right is empty,
          (tree :l)                            ;;return left
          (let [min (find-min (tree :r))]    ;otherwise swap next root up
            (assoc tree :v min :r (rm-node (tree :r) min)))))))

(defn in-order
  "with builtin function tree-seq"
  [tree]
  (->>  (tree-seq identity #(map % [:l :r]) tree)
        (remove nil?)
        (map :v)))

(defn look-up
  "loolup in a BST"
  ([tree v] (look-up tree v nil))
  ([tree v  not-found]
     (if tree
       (case (compare v (tree :v))
         1 (recur (tree :r) v not-found)
         -1 (recur (tree :l) v not-found)
         0 (tree :v))
       not-found)))

;; ex 2.67
(defn make-leaf
  "make a huffman leaf"
  [symbol weight]
  {:leaf? true :s  symbol :w weight})

(defn make-code-tree
  [left right]
  {:l left :r right
   :s (flatten (hash-set (:s left) (:s right)))
   :w (+ (:w left) (:w right))})
(defn choose-branch
  [bit branch]
  (cond (= bit 0) (:l branch)
        (= bit 1) (:r branch)
        :else (println "error orrured")))
(defn decode
  "decode the tree with bits"
  [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if-not (seq bits)
              '()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (:leaf? next-branch)
                  (cons (:s next-branch)
                        (decode-1 (rest bits) tree))
                  (recur (rest bits) next-branch)))))]
    (decode-1 bits tree)))



(defn make-leaf-set
  "make a sorted-set by built in clojure function assuming there won't
  be duplicate symbol"
  [pairs]
  (apply sorted-set-by
         (fn [p1 p2] (case (compare (second p1) (second p2))
                       0 (compare (first p1) (first p2))
                       -1 1
                       1 -1))
         pairs))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;=> (A D A B B C A)

;; ex 2.68
(def s-tree (zip/zipper #(not (:leaf? %))
                        (fn [node] (list (:l node) (:r node)))
                        (fn [node children] (assoc node :l (first children)
                                                   :r (second children)))
;                        identity
                        sample-tree))
(defn encode-symbol
  "encode an symbol with given tree."
  [symbol tree]
  )
