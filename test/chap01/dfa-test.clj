(ns test.chap01.dfa-test
  (:use clojure.contrib.test-is))
;  (:require src.chap01.dfa))

(deftest test-plus
  (is (= 4 (+ 2 2)) "Two plus two should be 4"))

(run-tests)


;; ITC page 37, Fig 1.8, M2 - easier.
;; dfa: L(M2) = {w | w ends in a 1}.
;(def m2-alph-list '(0 1))
;(def m2-trans-1 (make-transition "q1" m2-alph-list '("q1" "q2")))
;(def m2-trans-2 (make-transition "q2" m2-alph-list '("q1" "q2")))
;(def m2-trans-table (combine-trans m2-trans-1 m2-trans-2))
;(def m2-all-states '("q1" "q2"))
;(def m2-start-state "q1")
;(def m2-fin-sate (make-final-states "q2"))

;(def m2-dfa (make-dfa m2-all-states m2-alph-list m2-trans-table
 ;                     m2-start-state m2-fin-sate))

;; test data
;(println "\nL(M2)")
;(run-dfa '() m2-dfa)       ;; yes
;(run-dfa '(0) m2-dfa)      ;; no
;(run-dfa '(1) m2-dfa)      ;; yes
;(run-dfa '(0 0) m2-dfa)    ;; no
;(run-dfa '(1 1) m2-dfa)    ;; yes
;(run-dfa '(0 0 1) m2-dfa)  ;; yes

;; test data from the book (page 37)
;(run-dfa '(1 1 0) m2-dfa)    ;; no
;(run-dfa '(1 1 0 1) m2-dfa)  ;; yes