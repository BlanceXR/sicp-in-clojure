(ns sicp.chapter2-test
  "Short package description."
  (:use clojure.test
        sicp.chapter2))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 0))
    (is (= (deriv '(+ x y) 'x) 0))))
