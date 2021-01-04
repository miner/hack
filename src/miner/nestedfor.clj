;; (ns miner.nestedfor)

;;  NOT WORKING, just ideas>


(defn choose [cnt n]
  (mc/combinations (range cnt) n))

;; twice as fast for special case of 2
(defn choose2 [cnt]
  (for [a (range cnt)
        b (range (inc a) cnt)]
    [a b]))

(defn choose3 [cnt]
  (for [a (range cnt)
        b (range (inc a) cnt)
        c (range (inc b) cnt)]
    [a b c]))


;;; want a nested-for macro



;;; want a macro that creates N clauses of nested `for`
#_
(nested-for [v [a b c]]
            (range (inc (v -1)) cnt)
            (mapv v (range cnt))
            )

(nested-for [v cnt]
            (range (inc (v -1)) cnt)
            :let [foo (+ (v 0) (v -1))]
            :when (> foo 10)
            (map v 0 cnt)
            )

v = index of current clause var (like `for`)

(v :i) = i-th index [0..cnt)
(v -) = previous var
(v +) = next var

v#
v-1#

(v -1 0) = second arg is default if index would be out of range
