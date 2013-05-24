(ns sicp.chapter3
  "Short package description."
  (:refer-clojure :exclude [== + * / -])
  (:require [clojure.core :as core]
            [criterium.core]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :refer :all ]
            [alembic.still :as deps]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :exclude [sqrt] :refer :all]
            [clojure.tools.trace :refer :all]
            [clojure.pprint :refer :all]
            [clojure.repl :refer :all]
            [clojure.zip :as zip]))

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
  [deposit pw]
  (let [cops-count (atom 0)
        init-amount (atom deposit)
        withdraw (fn [amount] (if (>= (- @init-amount amount) 0)
                                (reset! init-amount (- @init-amount amount))
                                (throw (Exception. " : insufficent fund"))))
        deposit (fn [amount] (reset! init-amount (+ @init-amount amount)))]
    (fn dispatch [op password]
      (if (= password pw)
        (do (reset! cops-count 0)
            (condp = op
              'withdraw withdraw
              'deposit deposit
              (throw (Exception. " : no such operation"))))
        (do (swap! cops-count inc)
            (if (> @cops-count 7)
              (throw (Exception. ": 911!!!!!"))
              (throw (Exception. ": incorrect password"))))))))
