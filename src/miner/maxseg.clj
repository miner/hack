(ns miner.maxseg)

;;; https://github.com/Engelberg/automata/blob/master/src/automata/core.clj


;; THE CLASSIC INTERVIEW PROBLEM - MAXIMUM SEGMENT SUM A popular problem is to find an O(n)
;; algorithm for computing the maximum sum achievable by adding up some contiguous
;; subsequence (aka segment) of a sequence of numbers (typical input is a mix of positive
;; and negative integers).

;; For example,
;; => (maximum-segment-sum [-1 2 3 -4 5 -8 4])
;; 6
;; because 2+3+-4+5 is 6

;;; The trick is to keep a running sum as you traverse the sequence, 
;;; never letting the running sum dip below 0.
(defn maximum-segment-sum [s] 
  (apply max (reductions (comp #(max 0 %) +) 0 s)))



(def example [-1 2 3 -4 5 -8 4])

(def ex4 (into example cat [example example example]))

(def r100 (range 100))



;;; much slower, calculates a bunch of subvec sums
(defn maxss1 [s]
  (let [v (vec s)
        cnt (count v)]
    (apply max
           (for [i (range cnt)
                 j (range i cnt)]
             (apply + (subvec v i j))))))


;;; almost good
(defn maxss2 [s]
  (let [v (vec s)
        cnt (count v)]
    (reduce max 0
            (map (fn [end] (reduce (fn [sum x] (+ (max sum 0) x)) 0 (subvec v 0 end)))
                 (range 1 cnt)))))

;;; note:  (reduce max ...) is faster than (apply max ...)
;;; my single reduction keeps a stack of bests so far.  The current run is at the peek, but
;;; there maybe additional segment scores on the stack so you have to run max at the end.
;;; Slightly faster to check for non-negative x so you can pop bs and continue current run.

;;; fastest
(defn maxss [s]
  (reduce max
          (reduce (fn [bs x]
                    (let [b (peek bs)]
                      (if (neg? b)
                        (conj (pop bs) x)
                        (if (neg? x)
                          (conj bs (+ b x))
                          (conj (pop bs) (+ b x))))))
                  (list 0)
                  s)))

;;; tranducer version is also pretty good 
;;; actually faster on a big example
(defn maxsst [s]
  (transduce conj
             (fn ([bs x]
                  (let [b (peek bs)]
                    (if (neg? b)
                      (conj (pop bs) x)
                      (if (neg? x)
                        (conj bs (+ b x))
                        (conj (pop bs) (+ b x))))))
               ([bs] (reduce max bs)))
             (list 0)
             s))




;;; ----------------------------------------------------------------------

;;; But we're going to do something harder, we're looking for the maximum sum
;;; among subsequences that are *not* a continguous segment.

;;; For example,
;;; => (maximum-non-segment-sum [-1 4 5 -3 -4])
;;; 5

;;; codes an automat processing a bit mask (vector of 1/0s) and then boils it down to
;;; this amazingly consise function.  Read the original file for a detailed explanation.


(defn maximum-non-segment-sum-concise [s]
  (loop [s s, q1 Double/NEGATIVE_INFINITY, q2 Double/NEGATIVE_INFINITY,
         q3 Double/NEGATIVE_INFINITY]
    (if-let [n (first s)]
      (recur (next s) (max n (+ n q1)) (max q1 q2) (max (+ q2 n) q3 (+ q3 n))) 
      q3)))


#_   (maximum-non-segment-sum-concise [-1 4 5 -3 -4])
;;;=> 5


(def tricky [-3 -4 3 4 5 6 7])

(def tricky2 [-3 -4 3 4 5 6 7 -5 -1 -4 1])

(def tricky3 [1 3 4 5 6 7 -5 -1 -4])

;;; for max non-segment
(defn test-mnon [mnon]
  (assert (= (mnon tricky) 22))
  (assert (= (mnon tricky2) 26))
  (assert (= (mnon tricky3) 25))
  (assert (nil? (mnon [])))
  (assert (nil? (mnon [1])))
  (assert (= (mnon [-1 2 3 -4]) 2))
  (assert (= (mnon (range 10)) 44))
  true)





;;; SEM: if you only want ints, I think Integer/MIN_VALUE would look better.  Can't use
;;; Long/MIN_VALUE because it might overflow if you try adding a negative.  Also, avoid
;;; auto-boxing.  OK, I get why they used NEGATIVE_INFINITY to be more correct.  By mine is
;;; faster!


(defn max-non [s]
  (when (>= (bounded-count 3 s) 3)
    (loop [s s, q11 Integer/MIN_VALUE, q12 Integer/MIN_VALUE, q13 Integer/MIN_VALUE]
      (if-let [n (first s)]
        (recur (next s) (long (max n (+ n q11))) (long (max q11 q12))
               (long (max (+ q12 n) q13 (+ q13 n))))
        q13))))




;;; Book:  "Pearls of Functional Algorithm Design"
;;; 11 - Not the maximum segment sum

;;; ----------------------------------------------------------------------
;;; https://stackoverflow.com/questions/21033548/maximum-non-segment-sum

;;; Haskell answer I don't understand
;;; q0 (start)
;;; q1 (segmentEndingHere), state q2 (segmentNotEndingHere), or state q3 (nonSegment)

;;; my interpretation
;;; scan each number
;;; you can take it or not, which changes state, and makes a partial sum
;;; best move to extend from highest partial sum (like A*)
;;; but maybe calc all legal and hold paths (like breadth first search)
;;; ended up calc state and keep total, not path
;;; umaxn is even better as it keeps the best of each kw state in a map as it goes.

;;; another idea is you can know "best possible remaining score" from each position and
;;; pursue best then check if there was a hole.  (Didn't do this.)


;;; My best effort is umaxn which is based on the finite-state machine approach.


;;; use kw for states -- :start :begin :hole :ok
;;; case on st
;;; keep multiple states
;;; mapcat step with x

(defn expand-state [[score st :as sst] x]
  (case st
    :start (list [(+ score x) :begin] sst)
    :begin (list [(+ score x) :begin] [score :hole])
    :hole (list [(+ score x) :ok] sst)
    :ok (if (pos? x) (list [(+ score x) :ok]) (list sst))))

(defn rmaxn [s]
  (reduce max Long/MIN_VALUE
          (mapv first
               (filterv #(= :ok (peek %))
                        (reduce (fn [states x]
                                  (mapcat #(expand-state % x) states))
                               [[0 :start]]
                               s)))))

;;; new idea -- keep state in map with keys and scores.  Similar logic but only keep best of
;;; whatever kw state.  Works and much faster.  However, a bit tricky on the update logic as
;;; some transitions hit the same kw so you can't assume only one change per item.  I think
;;; this map-style state works because the transitions are simple and linear.  It probably
;;; wouldn't work if states could cycle.  So this is specialized for the particular problem.
;;; My much slower mapcat vector states used in rmaxn is a more general approach.

(defn maxnil [a b]
  (if (nil? a) b (max a b)))

;;; note: we did not need to use :start as you can always :begin with any item
(defn update-state [stm x]
  (let [{:keys [begin hole ok]} stm]
    (cond-> (update stm :begin maxnil x)
      begin  (update :hole maxnil begin)
      begin  (update :begin maxnil (+ begin x))
      hole   (update :ok maxnil (+ hole x))
      (and ok (pos? x))  (update :ok max (+ ok x)))))


(defn umaxn [s]
  (:ok (reduce update-state {} s)))


;;; Now, this is my favorite.  Not as fast as "concise" version, but pretty good.
;;; all-in-one version

(defn max-non-seg-sum [s]
  (let [maxnil (fn [a b] (if (nil? a) b (max a b)))
        update-max (fn [m k x] (update m k maxnil x))]
    (:ok (reduce (fn [{:keys [begin hole ok] :as stm} x]
                   (cond-> (update-max stm :begin x)
                     begin  (update-max :hole begin)
                     begin  (update-max :begin (+ begin x))
                     hole   (update-max :ok (+ hole x))
                     (and ok (pos? x))  (update :ok max (+ ok x))))
                 {}
                 s))))

;;; optimizing but limited to values greater than Integer/MIN_VALUE.  Might be better to use
;;; Double/NEGATIVE_INFINITY as done by Engelberg.

(defn mnss [s]
  (when (>= (bounded-count 3 s) 3)
    (let [res (peek (reduce (fn [[begin hole ok] x]
                              [(max begin x (+ begin x))
                               (max begin hole)
                               (max ok (+ ok x) (+ hole x))])
                            [Integer/MIN_VALUE Integer/MIN_VALUE Integer/MIN_VALUE]
                            s))]
      (when (> res Integer/MIN_VALUE)
        res))))

(defn mnss2 [s]
  (let [res (peek (reduce (fn [[begin hole ok] x]
                            [(max begin x (+ begin x))
                             (max begin hole)
                             (max ok (+ ok x) (+ hole x))])
                          [Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY
                           Double/NEGATIVE_INFINITY]
                          s))]
    (when-not (infinite? res)
      res)))


;;; ----------------------------------------------------------------------
;;; SEM: a bunch of junk below here is me just flailing with the non-seg problem.
;;; Go back to the original to see the right way to approach this problem.

(defn posis [s]
  (let [vvv (vec s)
        itot (reduce-kv (fn [itotal i x]
                          (if (pos? x)
                            (conj (conj (pop itotal) i) (+ (peek itotal) x))
                            itotal))
                        [0]
                        vvv)
        is (pop itot)
        tot (peek itot)]
    itot))


;;  (and (> (bounded-count 3 s) 2)

(defn conseq? [s]
  (when-let [s (seq s)]
    (boolean (reduce (fn [r i] (if (= r i) (inc i) (reduced false)))
                     (inc (first s))
                     (rest s)))))

(defn add-outsider [vvv pv]
  (let [before (range (dec (nth pv 0)))
        after (range (+ (peek pv) 2) (count vvv))]
    (when (or (seq before) (seq after))
      (let [outsider (apply max-key vvv (concat before after))]
        (reduce + 0 (mapv vvv (conj pv outsider)))))))


(defn punch-hole [vvv pv]
  (let [icnt (count pv)]
    (when (>= icnt 3)
      (let [hole (apply min-key vvv (subvec pv 1 (dec icnt)))]
        (reduce (fn [r p]
                  (if (= p hole)
                    r
                    (+ r (vvv p))))
                0
                pv)))))


;;; but you can also punch a hole and maybe still add an outsider
;;; also trim to add outsiders

;;; almost but not there for trimming
(defn maxn1 [s]
  (let [vvv (vec s)
        pv (reduce-kv (fn [rs i x]
                        (if (not (neg? x))
                          (conj rs i)
                          rs))
                      []
                      vvv)]
      (if (conseq? pv)
        (do (println "conseq" pv)
            (let [outsider (add-outsider vvv pv)
                  holed (punch-hole vvv pv)]
              ;; still need to consider trim + outsider
              (println "outsider" outsider "holed" holed)
              (if (and holed outsider)
                (max holed outsider)
                (or holed outsider))))

        (do (println "pv" pv)
            (reduce + (mapv vvv pv))))))



;;; buggy -- almost but misses some tricks -- do not use
(defn maxn-buggy [s]
  (let [vvv (vec s)
        pv (reduce-kv (fn [rs i x]
                        (if (not (neg? x))
                          (conj rs i)
                          rs))
                      []
                      vvv)]
      (if (conseq? pv)
        (do #_ (println "conseq" pv)
            (if-let [holed (punch-hole vvv pv)]
              holed
              (let [outsider (add-outsider vvv pv)
                    out1 (add-outsider vvv (subvec pv 1))
                    out2 (add-outsider vvv (subvec pv 0 (dec (count pv))))]
                ;; still need to consider trim + outsider
                ;; some trims might be better than outsider!
                (max (or outsider -999) (or out1 -999) (or out2 -999)))))

        (do #_ (println "pv" pv)
            (reduce + (mapv vvv pv))))))

(defn holes [s]
  (let [vvv (vec s)
        cnt (count vvv)]        
    (for [h (range 1 (dec cnt))]
      (assoc vvv h nil))))


;;; assumes that nils have been combined by rnegv  -- not a great idea
(defn hole? [v]
  (boolean (some (fn [[a b c]] (and (nil? b) a c)) (partition 3 1 v))))


;;; should make a variant of hole? that works like bhole? and can tolerate multiple nil/negs




(defn add [a b]
  (if b (+ a b) a))


;;; remove negs and combine into one nil
(defn rnegv [s]
  (reduce (fn [rv x]
            (let [prev (peek rv)]
              (cond (and (nil? prev) (neg? x)) rv
                    (neg? x) (conj rv nil)
                    :else (conj rv x))))
          []
          (drop-while neg? s)))



(defn pholes [s]
  (let [vvv (rnegv s)
        sum (reduce add 0 vvv)
        cnt (count vvv)]
    (if (hole? vvv)
      (conj vvv sum)
      (apply max-key peek
             (for [h (range cnt)
                   :when (vvv h)
                   :let [hv (assoc vvv h nil)]
                   :when (hole? hv)]
               (conj hv (- sum (vvv h))))))))



;;; punching holes is doomed as you can always have multiple holes
(defn hole-pats-BAD [cnt]
  (let [vvv (vec (range cnt))]
    (concat 
     (for [h (range 1 (dec cnt))]
       (assoc vvv h nil))
     (for [h (range 1 (- cnt 2))]
       (assoc vvv h nil (inc h) nil)))))


(defn grow [pat]
  [(conj pat 0)
   (conj pat 1)
   (into [0] pat)
   (into [1] pat)])

;;; works only for single hole, need to expand for multiple holes together
;;; doesn't seem practical
(defn grow-holes [cnt]
  (loop [hs [[1 0 1]]]
    (if (= (count (peek hs)) cnt)
      hs
      (recur (into [] (mapcat grow) hs)))))


;;; generating from long bits
;;; st -> false, 1, nil, true
(defn bhole? [cnt n]
  (true? (reduce (fn [st i]
            (let [b (bit-test n i)]
            (if (false? st)
              (if b 1 false)
              (if (= st 1)
                (if b 1 nil)
                (if (nil? st)
                  (if b (reduced true) nil)
                  false)))))
          false
          (range cnt))))


;;; another idea for bhole?
;;; for bit seq a to b (inclusive), a and b must be set, and popcount of (a+1)...(b-1) must
;;; be less than (b-a-2)
;;; so first find lowest one bit (a)
;;; highest one bit (b)
;;; then popcount between


;;; bithole? is faster and simpler than bhole?

(defn bithole? [n]
  (and (> (Long/highestOneBit n) (bit-shift-left (Long/lowestOneBit n) 1))
       (< (+ (Long/bitCount n)
             (Long/numberOfTrailingZeros n)
             (Long/numberOfLeadingZeros n))
          Long/SIZE)))



;;; not practical for larger counts, need 2^cnt bmasks

;;; cnt is the number of items
;;; 2r101 = 5, which is the smallest non-seg bit pattern
(defn bholes1 [cnt]
    (for [n (range 2r101 (bit-shift-left 1 cnt))
          :when (bhole? cnt n)]
      n))

;;; faster with bithole? but not practical
(defn bholes2 [cnt]
    (for [n (range 2r101 (bit-shift-left 1 cnt))
          :when (bithole? n)]
      n))

;;; faster but not lazy
(defn bholes [cnt]
  (filterv bithole? (range (bit-shift-left 1 cnt))))


(defn bscore [vvv bmask]
  (reduce-kv (fn [r i x]
               (if (bit-test bmask i)
                 (+ r x)
                 r))
             0
             vvv))

;;; not practical!
(defn bmaxn [s]
  (let [vvv (vec s)
        cnt (count vvv)]
    (reduce max 0 (map #(bscore vvv %) (bholes cnt)))))



;;; false at start, find a pos => 1, then nil, then pos => true
(defn has-hole? [v]
  (boolean (reduce (fn [st x]
                     (if (false? st)
                       (if (nil? x) false 1)
                       (if (nil? st)
                         (if (nil? x) nil (reduced true))
                         ;; 1 st
                         (if (nil? x) nil st))))
                   false
                   v)))

(defn best-case-nss [s]
  (let [bestv (reduce (fn [vvv x] (conj vvv (when (pos? x) x))) [] s)]
    (if (has-hole? bestv)
      (conj bestv (reduce + 0 (remove nil? bestv)))
      (conj bestv nil))))


;;; be careful: 0 is ambiguous, you can start with actual 0 or you can ignore with 0
;;; replacing a negative.  Can't use sign for state because of 0

;;; Remember most of this stuff is junk.  Look at my umaxn for my best answer.  The official
;;; "consise" is amazingly optimized.  I did not figure that out exactly.

