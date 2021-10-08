(ns miner.eknightcapture)



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




(defn bmoves [r f]
  (reduce (fn [b [rdiff fdiff]]
            (let [r2 (+ r rdiff)
                  f2 (+ f fdiff)]
              (if (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                (bit-set b (bit-or (bit-shift-left r2 3) f2))
                b)))
          0
          [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]]))


(def all-bmoves
  (into [] (for [rank (range 8)
                 file (range 8)]
             (bmoves rank file))))

(defn bcoords1 [rf]
  (vector (quot rf 8) (rem rf 8)))

;; slightly faster
(defn bcoords [rf]
  (vector (unsigned-bit-shift-right rf 3) (bit-and 7 rf)))

(defn kbmoves [k]
  (bmoves (unsigned-bit-shift-right k 3) (bit-and 7 k)))

(defn bseq [^long n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/highestOneBit n)]
        (recur (bit-and-not n h) (conj bs (Long/numberOfTrailingZeros h)))))))

(defn bfirst [^long n]
  (when-not (zero? n)
    (Long/numberOfTrailingZeros n)))

(defn brest [^long n]
  (when-not (zero? n)
    (bit-and-not n (Long/lowestOneBit n))))


;; faster with all possible moves precomputed
;;; use bits to make the kset, much faster than Clojure set, but of course less flexible

;;; by far the fastest -- but see bcaps4 for improvement
(defn bcaps [board]
  (let [kset (reduce-kv (fn [res r rank]
                          (reduce-kv (fn [res f x]
                                       (if (pos? x)
                                         (bit-set res (+ (* 8 r) f))
                                         res))
                                     res
                                     rank))
                        0
                        board)]
    ;;(println kset)
    (transduce (take-while #(> (Long/bitCount ^long %) 1))
               (fn ([res ^long ks]
                    (let [k (Long/numberOfTrailingZeros ks)
                          cok (bcoords k)]
                      (into res
                            (map #(hash-set cok (bcoords %)))
                            (bseq (bit-and ks (all-bmoves k))))))
                 ([res] res))
               []
               (iterate #(bit-xor % (Long/lowestOneBit ^long %)) kset))))

;; slightly faster
(defn bcaps3 [board]
  (transduce (take-while #(> (Long/bitCount ^long %) 1))
             (fn ([res ^long ks]
                  (let [k (Long/numberOfTrailingZeros ks)
                        cok (bcoords k)]
                    (into res
                          (map #(hash-set cok (bcoords %)))
                          (bseq (bit-and ks (all-bmoves k))))))
               ([res] res))
             []
             (iterate #(bit-xor % (Long/lowestOneBit ^long %))
                      (reduce-kv (fn [res r rank]
                                   (reduce-kv (fn [res f x]
                                                (if (pos? x)
                                                  (bit-set res (+ (* 8 r) f))
                                                  res))
                                              res
                                              rank))
                                 0
                                 board))))


;; 2x slower to compute moves on the fly with kbmoves, but still faster than non-bit approach
;; [code elided]


(defn bcaps4 [board]
  (transduce (take-while #(> (Long/bitCount ^long %) 1))
             (fn ([res ^long ks]
                  (let [k (bfirst ks)
                        cok (bcoords k)]
                    (into res
                          (map #(hash-set cok (bcoords %)))
                          (bseq (bit-and ks (all-bmoves k))))))
               ([res] res))
             []
             (iterate brest
                      (reduce-kv (fn [res r rank]
                                   (reduce-kv (fn [res f x]
                                                (if (pos? x)
                                                  (bit-set res (bit-or (bit-shift-left r 3) f))
                                                  res))
                                              res
                                              rank))
                                 0
                                 board))))




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

;;; fastest -- self-contained, except for k-moves above
(defn bcaps5 [board]
  (let [bfirst  (fn [n] (Long/numberOfTrailingZeros ^long n))
        brest (fn [n] (bit-and-not n (Long/lowestOneBit ^long n)))
        bseq (fn [n]
               (loop [n n bs ()]
                 (if (zero? n)
                   bs
                   (let [h (Long/highestOneBit ^long n)]
                     (recur (bit-and-not n h) (conj bs (Long/numberOfTrailingZeros h)))))))
        coords (fn [k] (vector (unsigned-bit-shift-right k 3) (bit-and 7 k)))]
    (transduce (take-while #(> (Long/bitCount ^long %) 1))
               (fn ([res ks]
                    (let [k (bfirst ks)
                          kco (coords k)]
                      (into res
                            (map #(hash-set kco (coords %)))
                            (bseq (bit-and ks (k-moves k))))))
                 ([res] res))
               []
               (iterate brest
                        (reduce-kv (fn [res r rank]
                                     (reduce-kv (fn [res f x]
                                                  (if (pos? x)
                                                    (bit-set res (bit-or (bit-shift-left r 3) f))
                                                    res))
                                                res
                                                rank))
                                   0
                                   board)))))



;; test if bit-and-not is faster than bit-xor

;;; SEM FIXME -- make bmoves that keeps all the destinations as bits in a single long
;;;   bit-and to see the hits


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

