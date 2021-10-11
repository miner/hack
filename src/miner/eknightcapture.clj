(ns miner.eknightcapture)

;;; https://gist.github.com/ericnormand/783c8ce1d6f5720144a90d36ed9b03db

;;; Dueling Knights
;;;
;;; A chess board has nothing but knights and empty spaces. In this setup, the color of
;;; the knights doesn't matter. Each knight could potentially capture any other. Your job is
;;; to write a function to figure out which knights can capture each other, if any. A board
;;; is represented as a vector of vectors, each 8 elements long. A 0 represents an empty
;;; square. A 1 represents a knight. Your function should return a collection of pairs. The
;;; pairs represent the positions of the two knights that can capture each other.


;; SEM notes:
;; Fastest approach.  Convert rank and file (row and col) into 0-63 squares and mark
;; bits in single long.  The long is essentially a sorted-set of 0-63.  Just big enough to
;; hold a chess board.  For each starting square, cache the possible knight moves.  Then
;; compare set of knight starting positions to each set of possible moves.  Removing
;; previous knight as you go to avoid duplicate captures.  Covert back to 2D coords for
;; final results.


(def k-moves
  (into [] (for [r (range 8)
                 f (range 8)]
             (reduce (fn [b [rdiff fdiff]]
                       (let [r2 (+ r rdiff)
                             f2 (+ f fdiff)]
                         (if (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                           (bit-set b (bit-or (bit-shift-left r2 3) f2))
                           b)))
                     0
                     [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]]))))


;; fast but lots more code than other solutions
(defn captures [board]
  (let [bfirst  (fn [n] (Long/numberOfTrailingZeros  n))
        bpop (fn [n] (bit-xor n (Long/lowestOneBit  n)))
        bseq (fn [n]
               (loop [n n bs ()]
                 (if (zero? n)
                   bs
                   (let [h (Long/highestOneBit  n)]
                     (recur (bit-xor n h) (conj bs (Long/numberOfTrailingZeros h)))))))
        coords (fn [k] (vector (unsigned-bit-shift-right k 3) (bit-and 7 k)))
        bkset (reduce-kv (fn [res r rank]
                           (reduce-kv (fn [res f x]
                                        (if (pos? x)
                                          (bit-set res (bit-or (bit-shift-left r 3) f))
                                          res))
                                      res
                                      rank))
                         0
                         board)]
    (transduce (take (dec (Long/bitCount bkset)))
               (fn ([res ks]
                    (let [k (bfirst ks)
                          kco (coords k)]
                      (into res
                            (map #(hash-set kco (coords %)))
                            (bseq (bit-and ks (k-moves k))))))
                 ([res] res))
               []
               (iterate bpop bkset))))


;;; @steffan-westcott -- clever to only jump down, less work to avoid duplicate captures.
;;; About 5x slower than my bits.
(defn piece-ij [board]
  (set (mapcat (fn [i row] (keep-indexed #(when (= 1 %2) [i %1]) row))
               (range)
               board)))

(defn knight-jumps-down [[i j]]
  [[(inc i) (- j 2)]
   [(inc i) (+ j 2)]
   [(+ i 2) (dec j)]
   [(+ i 2) (inc j)]])

(defn sw-captures [board]
  (let [knights (piece-ij board)]
    (set (mapcat (fn [x] (keep #(when (knights %1) #{x %1}) (knight-jumps-down x)))
                 knights))))



;;; ----------------------------------------------------------------------





;;; Chess notations lists the "file" (A-H column) first and the "rank" (1-8 row) second.
;;; So all my notations should be fixed to be file/rank in that order.

;;; Consider using single long to hold both

(defn frlong [file rank]
  (-> 0
      (bit-set (+ file 8))
      (bit-set rank)))

(defn long-file [frl]
  (- 55 (Long/numberOfLeadingZeros ^long frl)))

(defn long-rank [frl]
  (Long/numberOfTrailingZeros ^long frl))


(defn kmove-set [[fx rx]]
  (into #{} (for [rd [2 1]
                  sr [+ -]
                  sf [+ -]
                  :let [fd (- 3 rd)
                        f (+ fx (sf fd))
                        r (+ rx (sr rd))]
                  :when (and (<= 0 f 7)
                             (<= 0 r 7))]
              [f r])))

(defn knight-coords1 [board]
  (for [r (range (count board))
        f (range (count board))
        :when (pos? (get-in board [r f]))]
    [r f]))


(defn knight-coords [board]
  (reduce-kv (fn [res r rank]
               (reduce-kv (fn [res f x]
                            (if (pos? x)
                              (conj res [r f])
                              res))
                          res
                          rank))
               nil
               board))
          


;;; FIXME finds same capture both ways
(defn caps [board]
  (let [ks (knight-coords board)]
    (into #{} (mapcat (fn [k] (map #(hash-set k %) (filter (kmove-set k) ks)))) ks)))
          
;; slightly faster  but more code
;; needs more testing, makes sure last knight is a capture or second to last
(defn caps2 [board]
  (loop [ks (knight-coords board)
         res []]
      (let [k (first ks)
            ks (rest ks)]
        (if (empty? ks)
          res
          (let [kms (kmove-set k)]
            (recur ks (into res (comp (filter kms) (map #(hash-set k %))) ks)))))))

;; prettier, in-between perf
(defn caps3 [board]
  (reduce (fn [res ks]
            (let [k (first ks)
                  kms (kmove-set k)]
              (into res (comp (filter kms) (map #(hash-set k %))) (rest ks))))
          []
          (take-while next (iterate rest (knight-coords board)))))


(defn caps4 [board]
  (transduce (take-while next)
             (fn ([res ks]
                  (let [k (first ks)
                        kms (kmove-set k)]
                    (into res (comp (filter kms) (map #(hash-set k %))) (rest ks))))
               ([res] res))
             []
             (iterate rest (knight-coords board))))





(defn kmoves1 [[fx rx]]
  (for [rd [2 1]
        sr [+ -]
        sf [+ -]
        :let [fd (- 3 rd)
              f (+ fx (sf fd))
              r (+ rx (sr rd))]
        :when (and (<= 0 f 7)
                   (<= 0 r 7))]
    [f r]))

;; it's worth checking bounds (and logical)
;; multi-arirty <= is a bit slower than multiple binary tests

;; much faster than FOR loop
(defn kmoves [[r f]]
  (keep (fn [[rdiff fdiff]]
          (let [r2 (+ r rdiff)
                f2 (+ f fdiff)]
            (when (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
              [r2 f2])))
        [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]]))


(defn kmoves-best-FOR [[fx rx]]
  (for [rd [2 1]
        sr [+ -]
        sf [+ -]
        :let [fd (- 3 rd)
              f (+ fx (sf fd))
              r (+ rx (sr rd))]
        :when (and (>= f 0) (>= r 0)
                   (< f 8) (< r 8))]
    [f r]))




;; a bit faster
(defn caps53 [board]
  (transduce (take-while #(> (count %) 1))
             (fn ([res ks]
                  (let [k (first ks)]
                    (into res (comp (filter ks) (map #(hash-set k %))) (kmoves k))))
               ([res] res))
             []
             (iterate #(disj % (first %)) (set (knight-coords board)))))


(defn caps54 [board]
  (transduce (take-while #(> (count %) 1))
             (fn ([res ks]
                  (let [k (first ks)]
                    (into res
                          (comp
                           (keep (fn [[rdiff fdiff]]
                                   (let [r2 (+ (nth k 0) rdiff)
                                         f2 (+ (nth k 1) fdiff)]
                                     (when (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                                       (ks [r2 f2])))))
                           (map #(hash-set k %)))
                          [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]])))
               ([res] res))
             []
             (iterate #(disj % (first %)) (set (knight-coords board)))))


;;; ALL IN ONE
(defn kcaps [board]
  (let [kset (reduce-kv (fn [res r rank]
                          (reduce-kv (fn [res f x]
                                       (if (pos? x)
                                         (conj res [r f])
                                         res))
                                     res
                                     rank))
                        #{}
                        board)]
    (transduce (take-while #(> (count %) 1))
               (fn ([res ks]
                    (let [k (first ks)]
                      (into res
                            (comp
                             (keep (fn [[rdiff fdiff]]
                                     (let [r2 (+ (nth k 0) rdiff)
                                           f2 (+ (nth k 1) fdiff)]
                                       (when (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                                         [r2 f2]))))
                             (filter ks)
                             (map #(hash-set k %)))
                            [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]])))
                 ([res] res))
               []
               (iterate #(disj % (first %)) kset))))



(defn lcaps [board]
  (let [rank-and-file (juxt long-rank long-file)
        kset (reduce-kv (fn [res r rank]
                          (reduce-kv (fn [res f x]
                                       (if (pos? x)
                                         (conj res (frlong f r))
                                         res))
                                     res
                                     rank))
                        #{}
                        board)]
    (transduce (take-while #(> (count %) 1))
               (fn ([res ks]
                    (let [k (first ks)
                          rfk (rank-and-file k)]
                      (into res
                            (comp
                             (keep (fn [[rdiff fdiff]]
                                     (let [r2 (+ (long-rank k) rdiff)
                                           f2 (+ (long-file k) fdiff)]
                                       (when (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                                         (frlong f2 r2)))))
                             (filter ks)
                             (map #(hash-set rfk (rank-and-file %))))
                            [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]])))
                 ([res] res))
               []
               (iterate #(disj % (first %)) kset))))




(defn xmoves1 [rx fx]
  (for [rd [2 1]
        sr [+ -]
        sf [+ -]
        :let [fd (- 3 rd)
              f (+ fx (sf fd))
              r (+ rx (sr rd))]
        :when (and (<= 0 f 7)
                   (<= 0 r 7))]
    (+ (* r 8) f)))

(defn xmoves [r f]
  (keep (fn [[rdiff fdiff]]
          (let [r2 (+ r rdiff)
                f2 (+ f fdiff)]
            (when (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
              (+ (* r2 8) f2))))
        [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]]))


(def all-xmoves
  (vec (for [rank (range 8)
             file (range 8)]
         (xmoves rank file))))

(defn coords [rf]
  (vector (quot rf 8) (rem rf 8)))

;; faster with all possible moves precomputed
(defn xcaps [board]
  (let [kset (reduce-kv (fn [res r rank]
                          (reduce-kv (fn [res f x]
                                       (if (pos? x)
                                         (conj res (+ (* 8 r) f))
                                         res))
                                     res
                                     rank))
                        #{}
                        board)]
    ;;(println kset)
    (transduce (take-while #(> (count %) 1))
               (fn ([res ks]
                    (let [k (first ks)
                          cok (coords k)]
                      (into res
                            (comp
                             (filter ks)
                             (map #(hash-set cok (coords %))))
                            (all-xmoves k))))
                 ([res] res))
               []
               (iterate #(disj % (first %)) kset))))


;; slower if xmoves are calculated on the fly
(defn xcaps1 [board]
  (let [kset (reduce-kv (fn [res r rank]
                          (reduce-kv (fn [res f x]
                                       (if (pos? x)
                                         (conj res (+ (* 8 r) f))
                                         res))
                                     res
                                     rank))
                        #{}
                        board)]
    ;;(println kset)
    (transduce (take-while #(> (count %) 1))
               (fn ([res ks]
                    (let [k (first ks)
                          cok (coords k)]
                      (into res
                            (comp
                             (filter ks)
                             (map #(hash-set cok (coords %))))
                            (xmoves (quot k 8) (rem k 8)))))
                 ([res] res))
               []
               (iterate #(disj % (first %)) kset))))





#_
(dotimes [i 8] (dotimes [j 8] (let [x (+ j (* 8 i))] (print (format "%3d" x)))) (println))

#_
[  0  1  2  3  4  5  6  7
   8  9 10 11 12 13 14 15
  16 17 18 19 20 21 22 23
  24 25 26 27 28 29 30 31
  32 33 34 35 36 37 38 39
  40 41 42 43 44 45 46 47
  48 49 50 51 52 53 54 55
  56 57 58 59 60 61 62 63  ]




(def bbb [[0 0 0 1 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 1 0 0 0]
          [0 0 1 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]
          [0 0 0 0 0 0 0 0]])

(defn set= [a b]
  (= (set a) (set b)))

(defn smoke-captures [captures]
  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 1 0 0 0 1 0 0]
                        [0 0 0 0 1 0 1 0]
                        [0 1 0 0 0 1 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 1 0 0 0 0 0 1]
                        [0 0 0 0 1 0 0 0]])
             []))
  ;; no captures

  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 1 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]])
             [#{[0 3] [2 4]}]))

  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 1 0 0 0]
                        [0 0 1 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]])
             [#{[0 3] [2 4]} #{[2 4] [3 2]}]))
  true)

