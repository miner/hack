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



;; All in one.  I consider this a good and readable solution.  Acceptable performance, but
;; it's not especially fast.
(defn sudoku-valid? [board]
  (let [valid-row? (fn [row] (= (sort row) [1 2 3 4 5 6 7 8 9]))]
    (and (every? valid-row? board)
         (every? valid-row? (apply sequence (map list) board))
         (every? valid-row? (for [r (range 3)  c (range 3)]
                              (mapcat #(subvec % (* c 3) (* (inc c) 3))
                                      (subvec board (* r 3) (* (inc r) 3))))))))


(defn sudoku-valid-red? [board]
  (let [valid-row? (fn [row] (zero? (reduce bit-flip 1022 row)))]
    (and (every? valid-row? board)
         (every? valid-row? (apply sequence (map list) board))
         (every? valid-row? (for [r (range 3)  c (range 3)]
                              (mapcat #(subvec % (* c 3) (* (inc c) 3))
                                      (subvec board (* r 3) (* (inc r) 3))))))))



;; using the blocks3 idea from Steffan Westcott, translated into transducers
;; looking for failure, hence nothing? reducing.
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
;; faster with the bit-reduction instead of sort
(def ^:const sud-bits 1022)

(defn xsudoku-valid-red? [board]
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
;; inspired me to do a transducer version that was pretty good (see xsudoku-valid?)
(defn westcott-valid? [mat]
  (let [transpose (fn [mat] (apply mapv vector mat))
        blocks3 (fn [mat] (->> mat (map #(partition 3 %)) transpose flatten (partition 9)))
        segment-valid? (fn [xs] (= (set xs) (set (range 1 10))))]
    (every? segment-valid? (concat mat (transpose mat) (blocks3 mat)))))




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





;; Very fast version with bit twiddling.

;; (set! *unchecked-math* :warn-on-boxed)

;; Based on the idea by g7s.  Bookkeeping is indexed by the value x.  Mark the bits for the
;; row, col, and box that the value appears in.  If you see it again, fail.  Assumes 1-9
;; values on a 9x9 board.  You could fool it with illegal numbers so you might enable the
;; assertion to be safer, but it would double the execution time.

;; bit-or with 1 shifts is faster than bit-set

;; refactored the offsets to save a little time

;; Benchmark about 4 us.
(defn sudoku-valid-bits? [board]
  (boolean (reduce-kv
            (fn [tracker ^long r row]
              (let [boxoff (+ 32 (* (quot r 3) 3))
                    rbit (bit-shift-left 1 (+ r 16))]
                (reduce-kv (fn [tracker ^long c x]
                             ;; (assert (<= 1 x 9))
                             (let [mask ^long (bit-or rbit
                                                      (bit-shift-left 1 c)
                                                      (bit-shift-left 1 (+ (quot c 3) boxoff)))
                                   bits ^long (nth tracker x)]
                               (if (zero? (bit-and mask bits))
                                 (assoc! tracker x (bit-or mask bits))
                                 (reduced (reduced false)))))
                           tracker
                           row)))
            (transient (vec (repeat 10 0)))
            board)))


;; (set! *unchecked-math* false)



;;; Extension idea, not implemented
;;; "Miracle Sudoku"
;;; has extra constraints on adjacent cells and knight's move away
;;; https://benjamincongdon.me/blog/2020/05/23/Solving-the-Miracle-Sudoku-in-Prolog/
