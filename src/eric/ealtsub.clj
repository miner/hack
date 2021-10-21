(ns eric.ealtsub)

;; https://gist.github.com/ericnormand/3466a145d2efdf0d0bdfbb893f980cbb

;; Longest Alternating Substring
;; 
;; Write a function that takes a string of digits and returns the longest substring that
;; contains only alternating odd/even digits. If two substrings have the same length, return
;; the one that appears first.


;; nicest
(defn longest-alt-subs-SEM [s]
  (let [chodd? (fn [ch] (case ch (\1 \3 \5 \7 \9) true false))]
    (if (< (count s) 2)
      s
      (->> (reduce (fn [res c]
                     (let [p (peek (peek res))]
                       (if (= (chodd? p) (chodd? c))
                         (conj res [c])
                         (conj (pop res) (conj (peek res) c)))))
                   [[(first s)]]
                   s)
           rseq
           (apply max-key count)
           (apply str)))))


;; much faster to access chars by index in S
(defn longest-alt-subs [s]
  (let [iodd? (fn [i] (case (.charAt ^String s ^int i) (\1 \3 \5 \7 \9) true false))
        len (.length ^String s)]
    (if (< len 2)
      s
      (let [[longstart longend start]
            (reduce (fn [lls i]
                      (if (not= (iodd? (dec i)) (iodd? i))
                        lls
                        (let [[longstart longend start] lls]
                          (if (> (- i start) (- longend longstart))
                            [start i i]
                            [longstart longend i]))))
                   [-1 -1 0]
                   (range 1 len))]
        (if (> (- len start) (- longend longstart))
          (subs s start)
          (subs s longstart longend))))))


;; not fast, but conventional
(defn zalt [s]
  (if (< (count s) 2)
    s
    (let [iodd? (fn [i] (odd? (+ (int (nth s i)) i)))]
      (->> (into []
                 (comp (partition-by iodd?)
                       (map #(vector (nth % 0) (inc (peek %)))))
                 (range (count s)))
           rseq
           (apply max-key (fn [[a b]] (- b a)))
           (apply subs s)))))





(defn chodd? [ch]
  (case ch
    (\1 \3 \5 \7 \9) true
    false))

(defn digit [ch]
  (- (int ch) (int \0)))

(defn lalt [s]
  (if (< (count s) 2)
    s
    (apply str (apply max-key count
                      (loop [cs (seq s)
                             podd? (not (chodd? (first s)))
                             res [[]]]
                        (if (seq cs)
                          (let [c (first cs)
                                co? (chodd? c)]
                            (recur (rest cs)
                                   co?
                                   (if (= podd? (not co?))
                                     (conj (pop res) (conj (peek res) c))
                                     (conj res [c]))))
                          (rseq res)))))))


;; fastest
(defn longest-alt-subs1 [s]
  (if (< (count s) 2)
    s
    (->> (loop [cs (seq s)
                podd? (not (chodd? (first s)))
                res [[]]]
           (if (seq cs)
             (let [c (first cs)
                   co? (chodd? c)]
               (recur (rest cs)
                      co?
                      (if (= podd? (not co?))
                        (conj (pop res) (conj (peek res) c))
                        (conj res [c]))))
             (rseq res)))
         (apply max-key count)
         (apply str))))






;; maybe cool but not necessary
(defn longest-alt-subs4 [s]
  (if (< (count s) 2)
    s
    (transduce identity
               (fn ([res c]
                   (let [p (peek (peek res))]
                     (if (= (chodd? p) (chodd? c))
                       (conj res [c])
                       (conj (pop res) (conj (peek res) c)))))
                 ([res] (apply str (apply max-key count (rseq res)))))
               [[(first s)]]
               s)))





(defn laltx [s]
  (if (< (count s) 2)
    s
    (loop [cs (seq s)
           podd? (not (chodd? (first s)))
           res [[]]]
      (if (seq cs)
        (let [c (first cs)
              co? (chodd? c)]
          (recur (rest cs)
                 co?
                 (if (= podd? (not co?))
                   (conj (pop res) (conj (peek res) c))
                   (conj res [c]))))
        res))))



;; (conj (take-while even? (map-indexed #(+ % (digit %2)) (subs s 1))) (digit (first s)))




(defn smoke-alt [longest-alt-subs]
  (assert (= (longest-alt-subs "") ""))
  (assert (= (longest-alt-subs "1") "1"))
  (assert (= (longest-alt-subs "123") "123"))
  (assert (= (longest-alt-subs "13") "1"))
  (assert (= (longest-alt-subs "122381") "2381"))
  (assert (= (longest-alt-subs "238112") "2381"))
  (assert (= (longest-alt-subs "223344") "23"))
  true)




;; @steffan-westcott  fast and short -- winner!
(defn sw-longest-alt-subs [s]
  (if (empty? s)
    ""
    (reduce (fn [a b] (max-key count b a))
            (subs s 0 1)
            (re-seq #"(?:[02468][13579])+[02468]?|(?:[13579][02468])+[13579]?" s))))

;; @jonasseglare  short but slower
(defn j-longest-alt-subs [s]
  (->> (range (count s))
       (partition-by #(even? (+ % (int (nth s %)))))
       (map #(subs s (first %) (inc (last %))))
       reverse
       (apply max-key count "")))


;; @alex-gerdom 
(defn ag-longest-alt-subs [s]
  (let [even-odd #"([02468][13579])*[02468][13579]?"
        odd-even #"([13579][02468])*[13579][02468]?"
        empty-str #""
        pattern  (re-pattern (str even-odd "|" odd-even "|" empty-str))
        alt-subs (->> (re-seq pattern s)
                      (map first))]
    (reduce (fn [longest el]
              (if (< (count longest) (count el))
                el
                longest))
     ""
     alt-subs)))
