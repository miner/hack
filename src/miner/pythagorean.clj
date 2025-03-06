(ns miner.pythagorean
  (:require [clojure.math.combinatorics :as mc]
            [clojure.math :as m]))


;; "Scanning for Pythagorean triplets in Clojure"
;; https://clojure-diary.gitlab.io/2023/02/15/scanning-for-pythagorean-triplets-in-clojure.html
;; find_py_triplets.clj

;; Mostly bad example.  My solution is much simpler and faster.

;; SEM: note the correct spelling is "Pythagorean".  The orginal had it wrong.

;;; Note: wikipedia article has lots of interesting properties for Pythagorean Triples.
;;; Link below.

;;; Misspelled!
(defn pythogorean-triplet?
  "Returns `true` if passed arguments are pythogorean triplets.
   
   **Usage**
   
   ```clojure
   (pythogorean-triplet? 3 4 5)
   ```"
  [a b c]
  (let [[x y z] (sort [a b c])]
    (=
     (* z z)
     (+
      (* x x)
      (* y y)))))

(defn number-combinations
  "
   Take in a range and returns combinations of numbers
   
   **Usage**
   
   ```clojure
   (number-combinations (range 1 3)) ; #{(1 1 1) (2 2 2) (1 1 2) (1 2 2)}
   ```"
  [nums]
  (set
   (for [x nums
         y nums
         z nums]
     (sort (list x y z)))))

(defn filter-triplets [num-combinations]
  "
   Given number combinations, filters out one those are
   pythogorean triplets
   
   **Usage**
   
   ```clojure
   filter-triplets [[1 2 3] [5 3 4]]) ; ([5 3 4])
   ```"
  (filter
   #(pythogorean-triplet?
     (first %)
     (nth % 1)
     (last %))
   num-combinations))

(defn find-py-triplets
  "
   Finds all Pythogorean Triplets berween `start` and `end`
   numbers.
   
   **Usage**
   
   ```clojure
   (find-py-triplets 1 10) ; ((3 4 5) (6 8 10))
   ```
  "
  [start end]
  (let [numbers (range start (inc end))
        combinations (number-combinations numbers)
        triplets (filter-triplets combinations)]
    (sort-by first triplets)))



;;; ----------------------------------------------------------------------

;;; much simpler and faster to use clojure.math.combinatorics
;;; note: mc/combinations generates in lexical order so you know the greatest is last
;;; note: original "end" is inclusive, which is not the normal Clojure convention
(defn sem-triplets [start end]
  (filter (fn [[a b c]] (= (* c c) (+ (* a a) (* b b))))
          (mc/combinations (range start (inc end)) 3)))


;;; If you don't want to use m.c.c/combinations, you can create the triplets by hand like this.
;;; Note: still tricky to have inclusive "end".  Triplet (size 3) is hard-wired into generation.
;;; See my combo.clj for inspiration.  Not recommended.

;; faster for larger inputs
(defn sem-triplets3 [start end]
  (loop [i 2 res (map vector (range start (dec end)))]
    (if (zero? i)
      (filter (fn [[a b c]] (= (* c c) (+ (* a a) (* b b)))) res)
      (recur (dec i)
             (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) (inc end)))) res)))))



#_
(quick-bench (find-py-triplets 1 10))
;; Execution time mean : 303.599879 µs

#_
(quick-bench (sem-triplets 1 10))
;; Execution time mean : 1.064362 µs

#_
(require '[clojure.math.combinatorics :as mc])


;;; More stuff added much later.  Wikipedia article is useful.

;;; https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple

;;; "Primitive" means the triplet is not a multiple of a smaller triplet.  For example,
;;; [3 4 5] is primitive, but [6 8 10] is not primitive.

;;; note that generation order is not sorted so I defined another constant that is sorted.

(def primitive-pythagorean-triples-under-100
  [[3 4 5] [5 12 13] [8 15 17] [7 24 25] [20 21 29] [12 35 37] [9 40 41] [28 45 53]
   [11 60 61] [16 63 65] [33 56 65] [48 55 73] [13 84 85] [36 77 85] [39 80 89] [65 72 97]])

;;; sorted
(def prim-py-triples-100
  [[3 4 5] [5 12 13] [7 24 25] [8 15 17] [9 40 41] [11 60 61] [12 35 37] [13 84 85] [16 63 65]
   [20 21 29] [28 45 53] [33 56 65] [36 77 85] [39 80 89] [48 55 73] [65 72 97]])



(def primitive-pythagorean-triples-under-300
  (into primitive-pythagorean-triples-under-100
        [[20 99 101] [60 91 109] [15 112 113] [44 117 125] [88 105 137] [17 144 145]
         [24 143 145] [51 140 149] [85 132 157] [119 120 169] [52 165 173] [19 180 181]
         [57 176 185] [104 153 185] [95 168 193] [28 195 197] [84 187 205] [133 156 205]
         [21 220 221] [140 171 221] [60 221 229] [105 208 233] [120 209 241] [32 255 257]
         [23 264 265] [96 247 265] [69 260 269] [115 252 277] [160 231 281] [161 240 289] 
         [68 285 293]]))

;;; sorted
(def prim-py-triples-300
  [[3 4 5] [5 12 13] [7 24 25] [8 15 17] [9 40 41] [11 60 61] [12 35 37] [13 84 85]
   [15 112 113] [16 63 65] [17 144 145] [19 180 181] [20 21 29] [20 99 101] [21 220 221]
   [23 264 265] [24 143 145] [28 45 53] [28 195 197] [32 255 257] [33 56 65] [36 77 85]
   [39 80 89] [44 117 125] [48 55 73] [51 140 149] [52 165 173] [57 176 185] [60 91 109]
   [60 221 229] [65 72 97] [68 285 293] [69 260 269] [84 187 205] [85 132 157] [88 105 137]
   [95 168 193] [96 247 265] [104 153 185] [105 208 233] [115 252 277] [119 120 169]
   [120 209 241] [133 156 205] [140 171 221] [160 231 281] [161 240 289]])


;;; Formulas for generating Pythagorean triples
;;; https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples

(defn gcd [a b]
  (loop [a (abs a)
         b (abs b)]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defn coprime? [a b]
  (= (gcd a b) 1))


(defn pytrip? [[a b c]]
  (= (* c c) (+ (* a a) (* b b))))

(defn primtrip? [[a b c :as trip]]
  (and (pytrip? trip)
       (coprime? a c)))

(defn canonical-pytrip [[a b c :as trip]]
  (if (< a b)
    trip
    [b a c]))


;;; generate triples with all ints <= mx
;;; each triple is internall sorted (< a b c)
;;; but full list is not properly sorted by the generation
;;; not sure if :while is correct or maybe should just be :when, but seems to work
;;; not sure about limit for m but it seems OK, maybe could be half that?
;;; We use a local coprime? that assume positive inputs.
;;; It's a bit faster to integrate the odd? test into the n range rather than test later.
;;; Note: mx is a limit on the sides, but you need a large value to guarantee that all the
;;; smaller values have been generated.  That might matter if you're trying to generate and
;;; sort into a specific order.  If you just want any reasonable triples, pick a samll mx
;;; like 100 or 300.

(defn euclid-py-triples [mx]
  (let [coprime? (fn [a b]
                   (if (zero? b)
                     (= a 1)
                     (recur b (rem a b))))]
    (for [m (range 2 (long (m/ceil (m/sqrt mx))))
          n (if (odd? m) (range 2 m 2) (range 1 m 2))
          :when (coprime? m n)
          :let [a (- (* m m) (* n n))
                b (* 2 m n)
                c (+ (* m m) (* n n))]
          :while (and (<= c mx) (<= a mx) (<= b mx))]
      (if (< a b)
        [a b c]
        [b a c]))))


;;; make test fn for filtering triple such that all elements are less than or equal to N
(defn py-lte [n]
  (fn [[a b c]] (and (<= c n) (<= a n) (<= b n))))




;;; Tree form, using matrix multiplication
;;; https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples

;;; lazy but not in convenient order and not canonical triples (fixable)
;;; However, it's interesting that they prove the algoright covers all primitive triples
;;; (eventually).  If you pick a large cnt and filter with py-lte and map canonical-ptyrip
;;; and sort, you can probably get a reasonable result.

;;; berrgren-step is basically a matrix multiplication
;;; berrgren-layer expands the previous layer, lazy and infinite so you need to `take`

;; could change to canonical triples with this xform, but still unconventional order.
;; (comp cat (map canonical-pytrip))

(defn berrgren [cnt]
  (let [berrgren-step
        (fn [[a b c]]
          [[(+ a (* -2 b) (* 2 c)) (+ (* 2 a) (- b) (* 2 c)) (+ (* 2 a) (* -2 b) (* 3 c))]
           [(+ a (* 2 b) (* 2 c))  (+ (* 2 a) b (* 2 c))  (+ (* 2 a) (* 2 b) (* 3 c))]
           [(+ (- a) (* 2 b) (* 2 c))  (+ (* -2 a) b (* 2 c))  (+ (* -2 a) (* 2 b) (* 3 c))]])
        berrgren-layer (fn [layer] (sequence (mapcat berrgren-step) layer)) ]
    (sequence (comp cat (take cnt)) (iterate berrgren-layer [[3 4 5]]))))


(defn infinite-berrgren []
  (let [berrgren-step
        (fn [[a b c]]
          [[(+ a (* -2 b) (* 2 c)) (+ (* 2 a) (- b) (* 2 c)) (+ (* 2 a) (* -2 b) (* 3 c))]
           [(+ a (* 2 b) (* 2 c))  (+ (* 2 a) b (* 2 c))  (+ (* 2 a) (* 2 b) (* 3 c))]
           [(+ (- a) (* 2 b) (* 2 c))  (+ (* -2 a) b (* 2 c))  (+ (* -2 a) (* 2 b) (* 3 c))]])
        berrgren-layer (fn [layer] (sequence (mapcat berrgren-step) layer)) ]
    (sequence cat (iterate berrgren-layer [[3 4 5]]))))




;;; much more info
;;; https://r-knott.surrey.ac.uk/Pythag/pythag.html

;;; lots of integer sequences
;;; https://oeis.org/search?q=pythagorean&language=english&go=Search
