(defn make-transition [from-state alphabet-symbols to-states]
  "Returns a transition table (map) for a single state, from-state, that shows
the state in which the each of the alphabet symbols leads to.
The order of the alphabet symbols and the to-states is important.
The first symbol in the list of alphabet-symbols leads to the first state in
the the list of to-states if the machine was in the from-state."
  {(keyword from-state)
   (make-inner-transition (stringfy alphabet-symbols) to-states)})

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


(defn stringfy [symbols]
  "Returns a vector made of symbols that have been changed into strings."
  ((fn [symbols changed-symbols]
     (if (nil? symbols)
       changed-symbols
       (recur (rest symbols)
              (conj changed-symbols (str (first symbols))))))
   symbols []))

(defn combine-trans [& single-state-transitions]
  "Returns a table consisting of all the single state transitions."
  ((fn [single-state-transitions combo]
     (if (nil? single-state-transitions)
       combo
       (recur (rest single-state-transitions)
              (into combo single-state-transitions))))
  single-state-transitions {}))

(defn state-after-transition [from-state alph-symbol trans-table]
  (let [to-states (trans-table (keyword from-state))]
    (to-states (keyword alph-symbol))))

;; ITC Page 36, Fig 1.6, Finite automata M1
(def m1-trans-1 {:q1 {:0 "q2", :1 "q1"}})
(def m1-trans-2 {:q2 {:0 "q3", :1 "q2"}})
(def m1-trans-3 {:q3 {:0 "q2", :1 "q2"}})
(def m1-fin-state "q2")

;; ITC page 37, Fig 1.8, M2 - easier dfa: L(M2) = {w | w ends in a 1}
(def m2-trans-1 {:q1 {:0 "q1", :1 "q2"}})
(def m2-trans-2 {:q2 {:0 "q1", :1 "q2"}})
(def m2-all-states '("q1" "q2"))
(def m2-alph-list '(0 1))
(def m2-trans-table (combine-trans m2-trans-1 m2-trans-2))
(def m2-start-state "q1")
(def m2-fin-sate "q2")

(def m2-dfa (dfa m2-all-states m2-alph-list m2-trans-table
                 m2-start-state m2-fin-sate))

(defn dfa [all-states alph-list trans-table start-state final-states]
  {:all-state all-states
   :alph-list alph-list
   :trans-table trans-table
   :start-state start-state
   :final-states final-states})

;; if lang is empty, accept the lang
;; else
;;   take the first symbol of the lang
;;   we're in the start state
;;   find which state, q, the symbol leads to
;;     is q the terminal state?
;;     if yes, accept the lang
;;     else
;;       take the second symbol of the lang [we're in q]
;;

(defn run-dfa [language dfa]
  "Return accept if the machine recognises/accepts the language over the
alphabet."
  (if (empty? language)
    (println "accept lang: " language)
    (let [current-state (dfa :start-state)
          final-state (dfa :final-state)
          alph-list (dfa :alph-list)
          current-symbol (first (alph-list))]
