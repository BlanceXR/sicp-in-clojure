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
