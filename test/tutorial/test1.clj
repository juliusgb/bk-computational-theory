(ns tutorial.test1
  (:use clojure.contrib.test-is))

(deftest test-plus
  (is (= 4 (+ 7 2)) "Two plus two should be 4"))

(run-tests)
