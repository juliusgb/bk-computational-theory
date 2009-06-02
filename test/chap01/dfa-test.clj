(ns test.chap01.dfa-test
  (:use src.chap01.dfa)
  (:use clojure.contrib.test-is))

;; get the tests from src.chap01.dfa
;; see 0e1f523a9b0de5f5beb3640ee6776f20c64b82a1
(deftest test-m1
  "Uses automata M1, Fig 1.6, on page 36. L(M1) = {w | w contains at least
one 1 or an even number of 0s follow the last 1."
  (def m1-alph-list '(0 1))
  (def m1-trans-1 (make-transition "q1" m1-alph-list '("q1" "q2")))
  (def m1-trans-2 (make-transition "q2" m1-alph-list '("q3" "q2")))
  (def m1-trans-3 (make-transition "q3" m1-alph-list '("q2" "q2")))
  (def m1-trans-table (combine-trans m1-trans-1 m1-trans-2 m1-trans-3))
  (def m1-all-states '("q1" "q2" "q3"))
  (def m1-start-state "q1")
  (def m1-fin-state (make-final-states "q2"))
  (def m1-dfa (make-dfa m1-all-states m1-alph-list m1-trans-table
                        m1-start-state m1-fin-state))

  (let [empty-state {(keyword "accept empty input") "q1"}
        accept-state {(keyword "accept") "q2"}
        accept-msg "String ending with a 1 should be accepted in state q2."
        un-acceptable-state {(keyword "un-acceptable") "q1"}
        un-accept-msg "Un-accepted input should be in state q1."
        un-acceptable-state2 {(keyword "un-acceptable") "q3"}
        un-accept-msg2 "Un-accepted input should be in state q3."]
    (is (= empty-state
           (run-dfa '() m1-dfa))
        "Empty input should be accepted.")
    (is (= un-acceptable-state
           (run-dfa '(0) m1-dfa)))
    (is (= accept-state (run-dfa '(1) m1-dfa)) accept-msg)
    (is (= accept-state (run-dfa '(0 1) m1-dfa)) accept-msg)
    (is (= accept-state (run-dfa '(1 1) m1-dfa)) accept-msg)
    (is (= accept-state (run-dfa '(1 0 0) m1-dfa)) accept-msg)
    (is (= un-acceptable-state2 (run-dfa '(1 1 0) m1-dfa)) un-accept-msg2)))


(deftest test-m2
  "Uses automata M2, Fig 1.8, on page 37. L(M2) = {w | w ends in a 1}."
  (def m2-alph-list '(0 1))
  (def m2-trans-1 (make-transition "q1" m2-alph-list '("q1" "q2")))
  (def m2-trans-2 (make-transition "q2" m2-alph-list '("q1" "q2")))
  (def m2-trans-table (combine-trans m2-trans-1 m2-trans-2))
  (def m2-all-states '("q1" "q2"))
  (def m2-start-state "q1")
  (def m2-fin-sate (make-final-states "q2"))
  (def m2-dfa (make-dfa m2-all-states m2-alph-list m2-trans-table
                        m2-start-state m2-fin-sate))

  (let [empty-state {(keyword "accept empty input") "q1"}
        accept-state {(keyword "accept") "q2"}
        accept-msg "Accepted input should be in state q2."
        un-accept-state {(keyword "un-acceptable") "q1"}
        un-accept-msg "Un-acceptable input should be in state q1."]
    (is (= empty-state (run-dfa '() m2-dfa))
        "Empty input should be accepted")
    (is (= accept-state (run-dfa '(1) m2-dfa)) accept-msg)
    (is (= un-accept-state (run-dfa '(0) m2-dfa)) un-accept-msg)
    (is (= un-accept-state (run-dfa '(0 0) m2-dfa)) un-accept-msg)
    (is (= accept-state (run-dfa '(1 1) m2-dfa)) accept-msg)
    (is (= accept-state (run-dfa '(0 0 1) m2-dfa)) accept-msg)
    (is (= un-accept-state (run-dfa '(1 1 0) m2-dfa)) un-accept-msg)
    (is (= accept-state (run-dfa '(1 1 0 1) m2-dfa)) accept-msg)))


;; see 636516386e29b79111819872ac4616a23f6f60ba
(deftest test-m3
  "Uses automata M4 on Page 38, Example 1.9, Fig 10.
L(M3) = {w | w is empty string or ends in a 0}."
  (def m3-alph-list '(0 1))
  (def m3-trans-1 (make-transition "q1" m2-alph-list '("q1" "q2")))
  (def m3-trans-2 (make-transition "q2" m2-alph-list '("q1" "q2")))
  (def m3-trans-table (combine-trans m3-trans-1 m3-trans-2))
  (def m3-all-states '("q1" "q2"))
  (def m3-start-state "q1")
  (def m3-fin-sate (make-final-states "q1"))
  (def m3-dfa (make-dfa m3-all-states m3-alph-list m3-trans-table
                        m3-start-state m3-fin-sate))

  (let [empty-state {(keyword "accept empty input") "q1"}
        accept-state {(keyword "accept") "q1"}
        un-accept-state {(keyword "un-acceptable") "q2"}]
    (is (= empty-state (run-dfa '() m3-dfa)))
    (is (= accept-state (run-dfa '(1 1 0) m3-dfa)))
    (is (= un-accept-state (run-dfa '(1 1 0 1) m3-dfa))) ))

;; see 9a8e0f2a48119d875ce3c1f5e1c4a6be64a25a33.
(deftest test-m4
  "Page 38, Example 1.10, Figure 11.
L(M4) = {w | w starts and ends with a or that start and end with b}."

  (def m4-alph-list '("a" "b"))
  (def m4-trans-1 (make-transition "s" m4-alph-list '("q1" "r1")))
  (def m4-trans-2 (make-transition "q1" m4-alph-list '("q1" "q2")))
  (def m4-trans-3 (make-transition "q2" m4-alph-list '("q1" "q2")))
  (def m4-trans-4 (make-transition "r1" m4-alph-list '("r2" "r1")))
  (def m4-trans-5 (make-transition "r2" m4-alph-list '("r2" "r1")))
  (def m4-trans-table (combine-trans m4-trans-1 m4-trans-2 m4-trans-3
                                     m4-trans-4 m4-trans-5))
  (def m4-all-states '("s" "q1" "q2" "r1" "r2"))
  (def m4-start-state "s")
  (def m4-fin-states (make-final-states "q1" "r1"))

  (def m4-dfa (make-dfa m4-all-states m4-alph-list m4-trans-table
                        m4-start-state m4-fin-states))

  (let [empty-state {(keyword "accept empty input") "s"}
        empty-msg "Should accept empty input."
        accept-state1 {(keyword "accept") "q1"}
        accept1-msg "Accepted input should be in state q1."
        accept-state2 {(keyword "accept") "r1"}
        accept2-msg "Accepted input should be in state r1."
        un-accept-state1 {(keyword "un-acceptable") "r2"}
        un-accept-state2 {(keyword "un-acceptable") "q2"}]
    (is (= empty-state (run-dfa '() m4-dfa)) empty-msg)
    (is (= accept-state1 (run-dfa '("a") m4-dfa)) accept1-msg)
    (is (= accept-state2 (run-dfa '("b") m4-dfa)) accept2-msg)
    (is (= accept-state1 (run-dfa '("a" "a") m4-dfa)) accept1-msg)
    (is (= accept-state2 (run-dfa '("b" "b") m4-dfa)) accept2-msg)
    (is (= un-accept-state1 (run-dfa '("b" "a") m4-dfa)))
    (is (= un-accept-state2 (run-dfa '("a" "b") m4-dfa)))
    (is (= accept-state2 (run-dfa '("b" "a" "b") m4-dfa)) accept1-msg) ))

(run-tests)