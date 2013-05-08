(ns sicp.chapter2
  "Short package description."
  (:refer-clojure :exclude [== + * / -])
  (:require [clojure.core :as core]
            [criterium.core]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :refer :all ]))

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
