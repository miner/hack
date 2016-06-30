(ns miner.balance)

;; Re: Avoiding repetition while still using 'recur' on Clojure mailing list.
;; Here is my solution to 4clojure problem #177:
;; 
;; Write a function that takes in a string and returns truthy if all square [ ] round ( ) and
;; curly { } brackets are properly paired and legally nested, or returns falsey otherwise.


;; james@booleanknot.com solution
(defn valid-parens? [s]
  (let [opening-brackets {\( \), \[ \], \{ \}}
        closing-brackets (clojure.set/map-invert opening-brackets)]
    (loop [cs (seq s), stack []]
      (if-not cs
        (empty? stack)
        (let [c (first cs), cs (next cs)]
          (if (opening-brackets c)
            (recur cs (conj stack c))
            (if-let [b (closing-brackets c)]
              (if (= (peek stack) b)
                (recur cs (pop stack))
                false)
              (recur cs stack))))))))

;; loop version (above) is slightly faster than reduce

;; my reduce version
(defn rpar? [s]
  (let [mparen (apply array-map ")(}{][")
        opener (set (vals mparen))]
    (empty? (reduce (fn [stack c]
                      (if (opener c)
                        (conj stack c)
                        (if-let [b (mparen c)]
                          (if (= (peek stack) b)
                            (pop stack)
                            (reduced [false]))
                          stack)))
                    []
                    s))))

