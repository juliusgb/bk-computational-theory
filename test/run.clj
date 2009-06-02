(ns run
  (:use clojure.contrib.test-is)
  (:require src.chap01.dfa)
  (:require test.chap01.dfa-test))

(run-tests
 'test.tutorial.test)