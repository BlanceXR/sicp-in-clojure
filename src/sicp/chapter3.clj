(ns sicp.chapter3
  "Short package description."
  (:refer-clojure)
  (:require [clojure.core :as core]
            [criterium.core :as c]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mop ]
            [alembic.still :as deps]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :exclude [sqrt] :refer :all]
            [clojure.tools.trace :refer :all]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]
            [clojure.zip :as zip]
            [incanter.core :as ic])
  (:import (cern.colt.matrix.linalg.Algebra)
           (cern.colt.matrix impl.DenseDoubleMatrix2D
                             DoubleMatrix2D)))

;;; ex 3.1
(defn make-accumulator
  "make a mutable accumulator"
  [n]
  (let [n (atom n)]
    (fn [inc-num]
      (swap! n (partial + inc-num)))))

;;; ex 3.2
(defn make-monitored
  "monitor the function f"
  [f]
  (let [count (atom 0)]
    (fn [& args]
      (cond (= :how-many-calls? (first args)) @count
            (= :reset (first args)) (reset! count 0)
            :else (do (swap! count inc) (apply f args))))))

;;; ex 3.3 & ex 3.4
(defn make-account
  "make an bank account with password protection. Will call cops if
  enter 8 consecutive incorrect passwords in a row"
  [deposit first-password]
  (let [pw (atom #{first-password})
        cops-count (atom 0)
        init-amount (atom deposit)
        withdraw (fn [amount] (if (>= (- @init-amount amount) 0)
                                (reset! init-amount (- @init-amount amount))
                                (throw (Exception. " : insufficent fund"))))
        deposit (fn [amount] (reset! init-amount (+ @init-amount amount)))
        validate (fn [pw-validate] (@pw pw-validate))
        add-password (fn [new-pw] (do (swap! pw conj new-pw)
                                      :done))]
    (fn dispatch [op password]
      (if (@pw password)
        (do (reset! cops-count 0)
            (condp = op
              'withdraw withdraw
              'deposit deposit
              'add-password add-password
              (throw (Exception. " : no such operation"))))
        (do (swap! cops-count inc)
            (if (> @cops-count 7)
              (throw (Exception. ": 911!!!!!"))
              (throw (Exception. ": incorrect password"))))))))

;;; ex 3.7
(defn make-joint
  "create a new password for the account"
  [acc old-pw new-pw]
  ((acc 'add-password old-pw) new-pw))

;;; ex 3.8
(defn g [y]
  (let [y (atom y)]
    (fn [x] (let [z @y]
              (reset! y x)
              z))))
(def f (g 0))


;;; ex 3.23
;;; from https://gist.github.com/111147/548905621ef0ccdd6406ea3959a594cfd5956518
;;; by achim
(defn- inner-pop [i end]
  (condp = end
    :left  (subvec i 1)
    :right (pop i)))

(defn- inner-peek [i end]
  (condp = end
    :left  (first i)
    :right (peek i)))

(defn- inner-push [i x end]
  (condp = end
    :left  (vec (cons x i))
    :right (conj i x)))

(def empty-deque {})

(defn dpeek [d end]
  (cond (empty? d)  nil
        (:single d) (:single d)
        :else       (inner-peek (end d) end)))


(defn dpush [d x end]
  (let [order     ({:left identity, :right reverse} end)
        other-end ({:left :right :right :left} end)]
    (cond
      (empty? d)            {:single x}
      (:single d)           {end [x], :middle empty-deque, other-end [(:single d)]}
      (< (count (end d)) 4) (assoc d end (inner-push (end d) x end))
      :else                 (assoc d
                              end     (vec (order [x (inner-peek (end d) end)]))
                              :middle (dpush (:middle d) (inner-pop (end d) end) end)))))


(defn dpop [d end]
  (let [other-end ({:left :right :right :left} end)]
    (cond
      (empty? d)                  d
      (:single d)                 empty-deque
      (> (count (end d)) 1)       (assoc d end (inner-pop (end d) end))
      (not (empty? (:middle d)))  (assoc d
                                    end     (dpeek (:middle d) end)
                                    :middle (dpop (:middle d) end))
      (> (count (other-end d)) 1) (assoc d
                                    end       [(inner-peek (other-end d) end)]
                                    other-end (inner-pop (other-end d) end))
      :else                       {:single (inner-peek (other-end d) end)})))

(defn dseq [d]
  (lazy-seq
    (when-not (empty? d)
      (cons (dpeek d :left) (dseq (dpop d :left))))))

;;; ex 3.24

(defn make-table [same-key?]
  (partial sorted-map-by same-key?))

;;; ex 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
            (let ((previously-computed-result (lookup x table)))
              (or previously-computed-result
                  (let ((result (f x)))
                    (insert! x result table)
                    result))))))

(defn memoize-sicp
  "see core function : memoiza"
  [f]
  (let [table (atom {})]
    (fn [& x] (let [previously-computed-result (x @table)]
              (or previously-computed-result
                  (let [result (apply f x)]
                    (swap! table assoc x result)))))))

(def memo-fib
  (memoize (fn [n]
             (cond (= n 0) 0
                   (= n 1) 1
                   :else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2)))))))
