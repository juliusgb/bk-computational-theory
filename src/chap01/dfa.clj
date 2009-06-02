(ns src.chap01.dfa
  (:use clojure.contrib.test-is))

(defn make-inner-transition [alphabet-symbols to-states]
  "Returns a transition table (map) that gives the states that the alphabet
symbols lead to."
  ((fn [alphabet-symbols to-states current-states-trans]
     (if (nil? alphabet-symbols)
       current-states-trans
       (recur (rest alphabet-symbols)
              (rest to-states)
              (assoc current-states-trans
                (keyword (first alphabet-symbols)) (first to-states)))))
   alphabet-symbols to-states {}))


(defn make-transition [from-state alphabet-symbols to-states]
  "Returns a transition table (map) for a single state, from-state, that shows
the state in which the each of the alphabet symbols leads to.
The order of the alphabet symbols and the to-states is important.
The first symbol in the list of alphabet-symbols leads to the first state in
the the list of to-states if the machine was in the from-state."
  {(keyword from-state)
   (make-inner-transition (map str alphabet-symbols) to-states)})


(defn combine-trans [& single-state-transitions]
  "Returns a table consisting of all the single state transitions."
  ((fn [single-state-transitions combo]
     (if (nil? single-state-transitions)
       combo
       (recur (rest single-state-transitions)
              (into combo single-state-transitions))))
  single-state-transitions {}))


(defn state-after-transition [from-state alph-symbol trans-table]
  "Returns the next state the alph[abet] symbol leads to based on the
trans[ition] table."
  (let [to-states (trans-table (keyword from-state))]
    (to-states (keyword alph-symbol))))


(defn make-final-states [& final-states]
  "Return a set of final states."
  (set final-states))


(defn make-dfa [all-states alph-list trans-table start-state final-states]
  "Returns a dfa."
  {:all-state all-states
   :alph-list alph-list
   :trans-table trans-table
   :start-state (list "unprocessed" start-state)
   :final-states final-states})


(defn run-dfa-helper [inputs dfa]
  "Deals with all input except the empty input."
  ((fn [state inputs]
     (cond (and (not (contains? (dfa :final-states) state))
                (empty? inputs))
;             (str "un-acceptable - " state)
             {(keyword "un-acceptable") state}
           (and (contains? (dfa :final-states) state)
                (empty? inputs))
;             (str "accept - " state)
             {(keyword "accept") state}
           :else
             (recur (state-after-transition state
                                            (first inputs)
                                            (dfa :trans-table))
                    (rest inputs))))
   (dfa :start-state) (map str inputs)))


(defn run-dfa [inputs dfa]
  "Accepts the empty string as input."
  (let [proc-label (first (dfa :start-state))
        start-state (second (dfa :start-state))]
    (if (and (= "unprocessed" proc-label) (empty? inputs))
      {(keyword "accept empty input") start-state}
      (run-dfa-helper inputs (assoc dfa :start-state start-state)))))


;;-------------------------------------------------------------------------
;; TESTS
;; TODO: separate out these tests into their own file.
;;--------------------------------------------------------------------------


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


;; Page 38, Example 1.9, Fig 10.
;; L(M3) = {w | w is empty string or ends in a 0}.

(deftest test-m3
  "Uses automata M4 on Page 38, Example 1.9, Fig 10. L(M3) = {w | w is empty string or ends in a 0}."
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


;; Page 38, Example 1.10, Figure 11.
;; L(M4) = {w | w starts and ends with a or that start and end with b}.
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

(deftest test-m4
  (is (= "accept empty input"
         (run-dfa '() m4-dfa))
      "Empty input should be accepted.")
  (is (= "accept - q1"
         (run-dfa '("a") m4-dfa)))
  (is (= "accept - r1"
         (run-dfa '("b") m4-dfa)))
  (is (= "accept - q1"
         (run-dfa '("a" "a") m4-dfa)))
  (is (= "accept - r1"
         (run-dfa '("b" "b") m4-dfa)))
  (is (= "un-acceptable - q2"
         (run-dfa '("a" "b") m4-dfa)))
  (is (= "un-acceptable - r2"
         (run-dfa '("b" "a") m4-dfa)))
  (is (= "accept - r1"
         (run-dfa '("b" "a" "b") m4-dfa))))

(run-tests)