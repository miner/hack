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

;; using the blocks3 idea from Steffan Westcott, translated into transducers
;; FASTEST pretty good for transducers
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



(defn sv9? [board]
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






;; similar to mine but I'm lazier, I think, and faster.
;; sudoku-valid?
(defn g7s-valid?
       [rows]
       (let [valid   #(= (sort %) (range 1 10))
             slice-3 (partial partition 3)
             cols    (apply map vector rows)
             |3x3|   (->> rows
                          (map slice-3)
                          slice-3
                          (mapcat #(apply map concat %)))]
         (every? valid (concat rows cols |3x3|))))


;; hacked version of Steffan Westcott, singled function
;; concise and clever but slow
(defn westcott-valid? [mat]
  (let [transpose (fn [mat] (apply mapv vector mat))
        blocks3 (fn [mat] (->> mat (map #(partition 3 %)) transpose flatten (partition 9)))
        segment-valid? (fn [xs] (= (set xs) (set (range 1 10))))]
    (every? segment-valid? (concat mat (transpose mat) (blocks3 mat)))))




(defn sudval? [board]
  (and (every? valid-row? board)
       (let [flat (sequence cat board)]
         (every? valid-row? (map (fn [i] (take-nth 9 (drop i flat))) (range 9)))
         (let [threes (partition 3 flat)]
           (every? valid-row?
                   (sequence cat (for [i (range 3)]
                                   (map (fn [abc] (reduce into [] abc))
                                        (partition 3 (take-nth 3 (drop i threes)))))))))))


(defn sudval? [board]
  (and (every? valid-row? board)
       (let [flat (sequence cat board)]
         (every? valid-row? (map (fn [i] (take-nth 9 (drop i flat))) (range 9)))
         (let [threes (partition 3 flat)]
           (every? valid-row?
                   (sequence cat (for [i (range 3)]
                                   (map (fn [abc] (reduce into [] abc))
                                        (partition 3 (take-nth 3 (drop i threes)))))))))))


(defn ssq1 []
  (let [flat (vec (range 81))
        threes (partition 3 flat)]
    (sequence cat (for [i (range 3)]
                    (map (fn [abc] (reduce into [] abc))
                         (partition 3 (take-nth 3 (drop i threes))))))))


(defn ssq-bit-faster []
  (let [flat (vec (range 81))
        threes (partition 3 flat)]
    (map #(sequence cat %)
         (mapcat (fn [i]
                   (partition 3 (take-nth 3 (drop i threes))))
         (range 3)))))


(defn ssq-more-faster []
  (let [flat (vec (range 81))
        threes (partition 3 flat)]
    (into ()  (comp
               (mapcat (fn [i]
                         (into () (comp (drop i) (take-nth 3) (partition-all 3)) threes)))
               (map #(sequence cat %)))
          (range 3))))



(defn ssq []
  (let [flat (vec (range 81))
        threes (partition 3 flat)]
    (mapcat (fn [x3]
              (into ()
                    (comp
                     (take-nth 3)
                     (partition-all 3)
                     (map (fn [x33] (sequence cat x33))))
                    x3))
            (list threes (next threes) (nnext threes)))))





(defn ssqx []
  (let [flat (vec (range 81))
        xform (comp
               (partition-all 3)
               (take-nth 3)
               (partition-all 3)
               (map #(apply concat %))
               #_(map #(sequence cat %))
               )
        ]
    (mapcat #(into () xform %)
            (take 3 (iterate #(drop 3 %) flat)))))


(defn ssqx2 []
  (let [flat (vec (range 81))
        xform (comp
               (partition-all 3)
               (take-nth 3)
               (partition-all 3)
               (map #(reduce into [] %))
               )
        ]
    (mapcat #(into () xform %)
            (take 3 (iterate #(drop 3 %) flat)))))




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


(defn smoke-sud
  ([] (smoke-sud sudoku-valid?))
  ([sudoku-valid?]
   (let [good [[ 1 5 2 4 8 9 3 7 6 ]
               [ 7 3 9 2 5 6 8 4 1 ]
               [ 4 6 8 3 7 1 2 9 5 ]
               [ 3 8 7 1 2 4 6 5 9 ]
               [ 5 9 1 7 6 3 4 2 8 ]
               [ 2 4 6 8 9 5 7 1 3 ]
               [ 9 1 4 6 3 7 5 8 2 ]
               [ 6 2 5 9 4 8 1 3 7 ]
               [ 8 7 3 5 1 2 9 6 4 ]]
         bad  [[ 1 1 2 4 8 9 3 7 6 ]
               [ 7 3 9 2 5 6 8 4 1 ]
               [ 4 6 8 3 7 1 2 9 5 ]
               [ 3 8 7 1 2 4 6 5 9 ]
               [ 5 9 1 7 6 3 4 2 8 ]
               [ 2 4 6 8 9 5 7 1 3 ]
               [ 9 1 4 6 3 7 5 8 2 ]
               [ 6 2 5 9 4 8 1 3 7 ]
               [ 8 7 3 5 1 2 9 6 4 ]]]
     (assert (sudoku-valid? good))
     (assert (sudoku-valid? (transpose good)))
     (assert (not (sudoku-valid? bad) ))
     (assert (not (sudoku-valid? (transpose bad) ))))
   true))








(defn valid-row-slower? [v]
  (= (set v) #{1 2 3 4 5 6 7 8 9}))

