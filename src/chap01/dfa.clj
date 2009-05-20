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