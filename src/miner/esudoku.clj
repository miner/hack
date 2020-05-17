(ns miner.esudoku)

;; https://gist.github.com/ericnormand/75c04605d796efb4dcabecd4a7dcefcc

;; Sudoku Validator
;; board is 9x9 vectors


(defn vtranspose [v2d]
  (apply mapv vector v2d))

;; faster with transducers
(defn transpose [v2d]
  (into [] (apply sequence (map vector) v2d)))

;; fastest if you don't need actual vectors, just lists (seqs)
(defn xpose [v2d]
  (apply sequence (map list) v2d))

(defn sub-squares [board]
  (for [i (range 3)
        j (range 3)]
    (mapcat #(subvec % (* i 3) (* (inc i) 3))
            (subvec board (* j 3) (* (inc j) 3)))))

(defn valid-row? [row]
  (= (sort row) [1 2 3 4 5 6 7 8 9]))

(defn sudoku-valid1? [board]
  (and (every? valid-row? board)
       (every? valid-row? (transpose board))
       (every? valid-row? (sub-squares board))))



;; with my fixes, I think this is what he meant in the comment (by g7s)
;; but not faster on my machine
(defn svg7? [board]
  (let [valid-row? (fn [row] (= row #{1 2 3 4 5 6 7 8 9}))]
    (and (every? valid-row? (map set board))
         (every? valid-row? (apply sequence (map hash-set) board))
         (every? valid-row? (for [r (range 3) c (range 3)]
                              (set (mapcat #(subvec % (* c 3) (* (inc c) 3))
                                           (subvec board (* r 3) (* (inc r) 3)))))))))




;; all in one
(defn sudoku-valid? [board]
  (let [valid-row? (fn [row] (= (sort row) [1 2 3 4 5 6 7 8 9]))]
    (and (every? valid-row? board)
         (every? valid-row? (apply sequence (map list) board))
         (every? valid-row? (for [r (range 3)  c (range 3)]
                              (mapcat #(subvec % (* c 3) (* (inc c) 3))
                                      (subvec board (* r 3) (* (inc r) 3))))))))


(defn bs? [board]
  (let [valid-row? (fn [row] (zero? (reduce bit-flip 1022 row)))]
    (and (every? valid-row? board)
         (reduce (fn [res c]
                   (if (zero? (reduce (fn [col r]
                                 (bit-flip col (nth (nth board r) c)))
                               1022
                               (range 9)))
                     true
                     (reduced false)))
                 true
                 (range 9))
         (every? valid-row? (for [r (range 3)  c (range 3)]
                              (mapcat #(subvec % (* c 3) (* (inc c) 3))
                                      (subvec board (* r 3) (* (inc r) 3))))))))




(defn bs1? [board]
  (let [valid-row? (fn [row] (zero? (reduce bit-flip 1022 row)))]
    (and (every? valid-row? board)
         (reduce (fn [res c]
                   (if (zero? (reduce (fn [col row]
                                 (bit-flip col (nth row c)))
                               1022
                               board))
                     true
                     (reduced false)))
                 true
                 (range 9))

        (every? valid-row?  (partition 9 (for [bc (range 3)
               br (range 3)
               i (range (* bc 3) (* (inc bc) 3))
               j (range (* br 3) (* (inc br) 3))]
           (nth (nth board j) i)))))))




;; using the blocks3 idea from Steffan Westcott, translated into transducers
;; pretty good for transducers
;; about 90 us
(defn xsudoku-valid? [board]
  (let [transpose (fn [rows] (apply sequence (map list) rows))
        xfail1 (comp (map sort) (remove #(= % [1 2 3 4 5 6 7 8 9])) (take 1))
        nothing? (fn  ([] nil)  ([r] (nil? r))  ([r x] false))]
    (and (transduce xfail1 nothing? board)
         (transduce xfail1 nothing? (transpose board))
         (transduce (comp (partition-all 3)
                          (mapcat transpose)
                          cat
                          (partition-all 9)
                          xfail1)
                    nothing?
                    board))))


;; about 52 us
;; faster with the bit-reduction
(defn xbs? [board]
  (let [transpose (fn [rows] (apply sequence (map list) rows))
        xfail1 (comp (map #(reduce bit-flip 1022 %)) (remove zero?) (take 1))
        nothing? (fn  ([] nil)  ([r] (nil? r))  ([r x] false))]
    (and (transduce xfail1 nothing? board)
         (transduce xfail1 nothing? (transpose board))
         (transduce (comp (partition-all 3)
                          (mapcat transpose)
                          cat
                          (partition-all 9)
                          xfail1)
                    nothing?
                    board))))

(def ^:const sud-bits 1022)

(defn xbs3? [board]
  (let [transpose (fn [rows] (apply sequence (map list) rows))
        xfail1 (comp (map #(reduce bit-flip sud-bits %)) (remove zero?) (take 1))
        nothing? (fn  ([] nil)  ([r] (nil? r))  ([r x] false))]
    (and (transduce xfail1 nothing? board)
         (transduce xfail1 nothing? (transpose board))
         (transduce (comp (partition-all 3)
                          (mapcat transpose)
                          cat
                          (partition-all 9)
                          xfail1)
                    nothing?
                    board))))






;; hacked version of Steffan Westcott, singled function
;; concise and clever but slow
;; inspired me to do a transducer version that was pretty good
(defn westcott-valid? [mat]
  (let [transpose (fn [mat] (apply mapv vector mat))
        blocks3 (fn [mat] (->> mat (map #(partition 3 %)) transpose flatten (partition 9)))
        segment-valid? (fn [xs] (= (set xs) (set (range 1 10))))]
    (every? segment-valid? (concat mat (transpose mat) (blocks3 mat)))))



;; slowish
(defn sudval? [board]
  (and (every? valid-row? board)
       (let [flat (sequence cat board)]
         (every? valid-row? (map (fn [i] (take-nth 9 (drop i flat))) (range 9)))
         (let [threes (partition 3 flat)]
           (every? valid-row?
                   (sequence cat (for [i (range 3)]
                                   (map (fn [abc] (reduce into [] abc))
                                        (partition 3 (take-nth 3 (drop i threes)))))))))))




#_ ([0 0] [0 1] [0 2]
 [1 0] [1 1] [1 2]
 [2 0] [2 1] [2 2])

#_
[[ 0  1  2  3  4  5  6  7  8]
 [ 9 10 11 12 13 14 15 16 17]
 [18 19 20 21 22 23 24 25 26]
 [27 28 29 30 31 32 33 34 35]
 [36 37 38 39 40 41 42 43 44]
 [45 46 47 48 49 50 51 52 53]
 [54 55 56 57 58 59 60 61 62]
 [63 64 65 66 67 68 69 70 71]
 [72 73 74 75 76 77 78 79 80]]


(def sq (mapv vec (partition 9 (range 81))))

(def bb [[ 1 5 2 4 8 9 3 7 6 ]
         [ 7 3 9 2 5 6 8 4 1 ]
         [ 4 6 8 3 7 1 2 9 5 ]
         [ 3 8 7 1 2 4 6 5 9 ]
         [ 5 9 1 7 6 3 4 2 8 ]
         [ 2 4 6 8 9 5 7 1 3 ]
         [ 9 1 4 6 3 7 5 8 2 ]
         [ 6 2 5 9 4 8 1 3 7 ]
         [ 8 7 3 5 1 2 9 6 4 ]])

(def good [[ 1 5 2 4 8 9 3 7 6 ]
           [ 7 3 9 2 5 6 8 4 1 ]
           [ 4 6 8 3 7 1 2 9 5 ]
           [ 3 8 7 1 2 4 6 5 9 ]
           [ 5 9 1 7 6 3 4 2 8 ]
           [ 2 4 6 8 9 5 7 1 3 ]
           [ 9 1 4 6 3 7 5 8 2 ]
           [ 6 2 5 9 4 8 1 3 7 ]
           [ 8 7 3 5 1 2 9 6 4 ]])

(def dbad [[ 1 2 5 4 8 9 3 7 6 ]
           [ 7 3 2 9 5 6 8 4 1 ]
           [ 4 6 8 3 7 1 2 9 5 ]
           [ 3 8 7 1 2 4 6 5 9 ]
           [ 5 9 1 7 6 3 4 2 8 ]
           [ 2 4 6 8 9 5 7 1 3 ]
           [ 9 1 4 6 3 7 5 8 2 ]
           [ 6 5 9 2 4 8 1 3 7 ]
           [ 8 7 3 5 1 2 9 6 4 ]])


(def tgood (transpose good))

(def bad  [[ 1 1 2 4 8 9 3 7 6 ]
           [ 7 3 9 2 5 6 8 4 1 ]
           [ 4 6 8 3 7 1 2 9 5 ]
           [ 3 8 7 1 2 4 6 5 9 ]
           [ 5 9 1 7 6 3 4 2 8 ]
           [ 2 4 6 8 9 5 7 1 3 ]
           [ 9 1 4 6 3 7 5 8 2 ]
           [ 6 2 5 9 4 8 1 3 7 ]
           [ 8 7 3 5 1 2 9 6 4 ]])

(def tbad (transpose bad))



(defn smoke-sud
  ([] (smoke-sud sudoku-valid?))
  ([sudoku-valid?]
   (assert (sudoku-valid? good))
   (assert (sudoku-valid? tgood))
   (assert (not (sudoku-valid? dbad)))
   (assert (not (sudoku-valid? bad) ))
   (assert (not (sudoku-valid? tbad) ))
   true))

(require 'criterium.core)

(defn ben
  ([] (ben sudoku-valid?))
  ([& svfns]
   (doseq [sv svfns]
     (println)
     (println (str sv))
     (criterium.core/quick-bench (smoke-sud sv)))
   (println)))





(defn valid-row-slower? [v]
  (= (set v) #{1 2 3 4 5 6 7 8 9}))






;;;;;


(def init-tracker  (into [] (repeat 9 (vector-of :long 0 0 0))))

(defn g7s-fastest?
  [rows]
  (let [tracker
        (persistent!
         (reduce
          (fn [tracker [^long row-idx ^long col-idx]]
            (let [tr-idx        (dec ^long (nth (nth rows row-idx) col-idx))
                  box-idx       (+ (quot col-idx 3)
                                   (* (quot row-idx 3) 3))
                  [row col box] (get tracker tr-idx)]
              (if (or (bit-test row row-idx)
                      (bit-test col col-idx)
                      (bit-test box box-idx))
                (reduced (conj! tracker false))
                (assoc! tracker
                        tr-idx
                        (vector-of :long
                                   (bit-set row row-idx)
                                   (bit-set col col-idx)
                                   (bit-set box box-idx))))))
          (transient init-tracker)
          (for [row-idx (range 0 9) col-idx (range 0 9)]
            [row-idx col-idx])))]
    (boolean (peek tracker))))



;; doesn't matter much but type hints help a little
(set! *unchecked-math* :warn-on-boxed)


;; SEM idea condense 3 elements into one long
(def init-long-bits (into [] (repeat 9 0)))

(defn sem-fastest1?
  [rows]
  (let [tracker
        (persistent!
         (reduce
          (fn [tracker [^long row-idx ^long col-idx]]
            (let [tr-idx        (dec ^long (nth (nth rows row-idx) col-idx))
                  box-bit       (+ 18 (quot col-idx 3)
                                   (* (quot row-idx 3) 3))
                  col-bit (+ 9 col-idx)
                  tbits (nth tracker tr-idx)]
              (if (or (bit-test tbits row-idx)
                      (bit-test tbits col-bit)
                      (bit-test tbits box-bit))
                (reduced (conj! tracker false))
                (assoc! tracker
                        tr-idx
                        (-> tbits (bit-set row-idx) (bit-set col-bit) (bit-set box-bit))))))
          (transient init-long-bits)
          (for [row-idx (range 0 9) col-idx (range 0 9)]
            [row-idx col-idx])))]
    (boolean (peek tracker))))

(defn sem-fastest?
  [rows]
  (let [tracker
        (persistent!
         (reduce
          (fn [tracker [^long row-idx ^long col-idx]]
            (let [tr-idx        (dec ^long (nth (nth rows row-idx) col-idx))
                  box-bit       (+ 18 (quot col-idx 3)
                                   (* (quot row-idx 3) 3))
                  col-bit (+ 9 col-idx)
                  tbits (nth tracker tr-idx)]
              (if (or (bit-test tbits row-idx)
                      (bit-test tbits col-bit)
                      (bit-test tbits box-bit))
                (reduced (conj! tracker false))
                (assoc! tracker
                        tr-idx
                        (-> tbits (bit-set row-idx) (bit-set col-bit) (bit-set box-bit))))))
          (transient init-long-bits)
          (for [row-idx (range 0 9) col-idx (range 0 9)]
            [row-idx col-idx])))]
    (boolean (peek tracker))))






(defn sem-svb1 [board]
  ;; check rows fast
  (reduce-kv (fn [res ^long irow row]
               (if-not (zero? ^long (reduce bit-flip 1022 row))
                 (reduced false)
                 (reduce-kv (fn [r ^long icol x]
                              (let [ibox (+ 10 (quot icol 3) (* (quot irow 3) 3))
                                    bits (nth r x)]
                                (if (or (bit-test bits icol)
                                        (bit-test bits ibox))
                                  (reduced (reduced false))
                                  (assoc r x (-> bits (bit-set icol) (bit-set ibox))))))
                            res
                            row)))
             (vec (repeat 10 0))
             board))

(defn sem-svb11 [board]
  ;; check rows fast
  (reduce-kv (fn [res ^long irow row]
               (if-not (zero? ^long (reduce bit-flip 1022 row))
                 (reduced false)
                 (reduce-kv (fn [r ^long icol x]
                              (let [ibox (+ 10 (quot icol 3) (* (quot irow 3) 3))
                                    bits (nth r x)]
                                (if (or (bit-test bits icol)
                                        (bit-test bits ibox))
                                  (reduced (reduced false))
                                  (assoc! r x (-> bits (bit-set icol) (bit-set ibox))))))
                            res
                            row)))
             (transient (vec (repeat 10 0)))
             board))


(defn sem-svb2 [board]
  ;; check rows fast
  (reduce-kv (fn [res ^long irow row]
               (reduce-kv (fn [r ^long icol x]
                            (let [ibox (+ 10 (quot icol 3) (* (quot irow 3) 3))
                                  col-bit (+ 20 icol)
                                  bits (nth r x)]
                                (if (or (bit-test bits irow)
                                        (bit-test bits col-bit)
                                        (bit-test bits ibox))
                                  (reduced (reduced false))
                                  (assoc r x (-> bits
                                                 (bit-set irow)
                                                 (bit-set col-bit)
                                                 (bit-set ibox))))))
                            res
                            row))
             (vec (repeat 10 0))
             board))


(defn sem-svb? [board]
  (boolean (reduce-kv (fn [res ^long irow row]
               (reduce-kv (fn [r ^long icol x]
                            (let [ibox (+ 10 (quot icol 3) (* (quot irow 3) 3))
                                  col-bit (+ 20 icol)
                                  bits (nth r x)]
                                (if (or (bit-test bits irow)
                                        (bit-test bits col-bit)
                                        (bit-test bits ibox))
                                  (reduced (reduced false))
                                  (assoc! r x (-> bits
                                                 (bit-set irow)
                                                 (bit-set col-bit)
                                                 (bit-set ibox))))))
                            res
                            row))
             (transient (vec (repeat 10 0)))
             board)))



(defn sem-svb4? [board]
  (boolean (reduce-kv
            (fn [res ^long irow row]
              (reduce-kv (fn [res ^long col x]
                           (let [ibox (+ 16 (quot col 3) (* (quot irow 3) 3))
                                 icol (+ 32 col)
                                 bits (nth res x)]
                             (if (or (bit-test bits irow)
                                     (bit-test bits icol)
                                     (bit-test bits ibox))
                               (reduced (reduced false))
                               (assoc! res x (-> bits
                                                 (bit-set irow)
                                                 (bit-set icol)
                                                 (bit-set ibox))))))
                         res
                         row))
            (transient (vec (repeat 10 0)))
            board)))


(defn sem-svb5? [board]
  (boolean (reduce-kv
            (fn [res ^long irow row]
              (reduce-kv (fn [res ^long col x]
                           (let [ibox (+ 16 (quot col 3) (* (quot irow 3) 3))
                                 icol (+ 32 col)
                                 bits (nth res x)]
                             (if (or #_ (not (<= 1 x 9))
                                     (bit-test bits irow)
                                     (bit-test bits icol)
                                     (bit-test bits ibox))
                               (reduced (reduced false))
                               (assoc! res x (-> bits
                                                 (bit-set irow)
                                                 (bit-set icol)
                                                 (bit-set ibox))))))
                         res
                         row))
            (transient (vec (repeat 10 0)))
            board)))





;; bit-or with 1 shifts is faster than bit-sets

;; faster with bit twiddling.  Assumes on 1-9 values on a 9x9 board
(defn sudoku-valid-bits? [board]
  (boolean (reduce-kv
            (fn [res ^long r row]
              (reduce-kv (fn [res ^long c x]
                           (let [mask ^long (bit-or (bit-shift-left 1 (+ 16 r))
                                                    (bit-shift-left 1 (+ 32 c))
                                                    (bit-shift-left 1 (+ (quot c 3)
                                                                         (* (quot r 3) 3))))
                                 bits ^long (nth res x)]
                             (if (zero? (bit-and mask bits))
                               (assoc! res x (bit-or mask bits))
                               (reduced (reduced false)))))
                         res
                         row))
            (transient (vec (repeat 10 0)))
            board)))




(set! *unchecked-math* false)

(defn svbits? [board]
  (boolean (reduce-kv
            (fn [res ^long r row]
              (reduce-kv (fn [res ^long c x]
                           (let [mask ^long (bit-or (bit-shift-left 1 (+ 16 r))
                                                    (bit-shift-left 1 (+ 32 c))
                                                    (bit-shift-left 1 (+ (quot c 3)
                                                                         (* (quot r 3) 3))))
                                 bits ^long (nth res x)]
                             (if (zero? (bit-and mask bits))
                               (assoc! res x (bit-or mask bits))
                               (reduced (reduced false)))))
                         res
                         row))
            (transient (vec (repeat 10 0)))
            board)))
