(ns miner.euler08)

;;; post seen on Planet Clojure
;;; https://clojure-diary.gitlab.io/2024/11/11/project-euler-problem-8.html

(def data "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450")

(def num-str (clojure.string/replace data #"\n" ""))

(def digit-chars (partition 13 1 num-str))

(defn product [chars]
  (reduce * (map #(read-string (str %)) chars)))

(defn product-and-chars [chars]
  [(product chars) chars])

;;; original poster solution
(defn euler08 []
  (last (sort-by first (map product-and-chars digit-chars))))


;;; SEM comments
;;; wrong approach -- convert strings to numbers only once then work in the number space.
;;; Don't take a last, do your sort the right way to take a first.
;;; My result gives the digits as longs, not characters as the original did.

(defn sem-euler08 []
  (let [numch (fn [ch] (- (long ch) (long \0)))]
    (first (sort-by (comp - key)
                    (reduce (fn [m v] (assoc m (reduce * 1 v) v))
                            {}
                            (partitionv 13 1 (mapv numch num-str)))))))


;;; actually, you only need the max so just do the comparisons as you create the results
;;; This does a little conversion at the end to use the same result format as above.

(defn xeuler08 []
  (let [numch (fn [ch] (- (long ch) (long \0)))]
    (transduce (map #(conj % (reduce * 1 %)))
               (fn ([a b] (if (>= (peek a) (peek b)) a b))
                 ([r] (vector (peek r) (pop r))))
               [-1]
               (partitionv 13 1 (mapv numch num-str)))))

;;; much faster to use subvec
(defn ieuler08 []
  (let [numch (fn [ch] (- (long ch) (long \0)))
        numv (mapv numch num-str)]
    (reduce (fn [r i]
              (let [sv (subvec numv i (+ i 13))
                    p (reduce * sv)]
                (if (> p (peek r))
                  (conj sv p)
                  r)))
               [-1]
               (range (- (count numv) 12)))))

;;; I prefer to give result as vector of digits plus product (in peek/last position)

(defn seuler08 []
  (let [numch (fn [ch] (- (long ch) (long \0)))
        numv (mapv numch num-str)]
    (transduce (map #(subvec numv % (+ % 13)))
               (fn ([r sv]
                 (let [p (reduce * sv)]
                   (if (> p (peek r))
                     (conj sv p)
                     r)))
                 ([r] r))
               [-1]
               (range (- (count numv) 12)))))


;;; of course, it's much better to convert the domain to numbers just once
(def data-vec (mapv (fn [ch] (- (long ch) (long \0))) num-str))

(defn max-product-run [width data-vec]
  (transduce (map #(subvec data-vec % (+ % width)))
             (fn ([r sv]
                  (let [p (reduce * sv)]
                    (if (> p (peek r))
                      (conj sv p)
                      r)))
               ([r] r))
             [-1]
             (range (- (count data-vec) (dec width)))))

(defn eul9 []
  (max-product-run 13 data-vec))


;;; Makes me think it would be nice to have a partition variant that returns subvectors.
;;; Fast if you have an original vector.

;;; copied from partition.clj 
(defn subpartitionv
  ([n v] (subpartitionv n n v))
  ([n step v]
   (map #(subvec v % (+ % n)) (range 0 (- (count v) (dec n)) step)))
  ([n step pad v]
   (let [cnt (count v)]
     (cond (zero? cnt) ()
           (<= cnt n) (list (into v (take (- n cnt)) pad))
           :else (concat (subpartitionv n step v)
                         (lazy-seq
                          (let [padlen (- step (rem (- cnt n) step))]
                            (when (< padlen n)
                              (list (into (subvec v (- cnt (- n padlen)))
                                          (take padlen) pad))))))))))


(defn max-prod [width data-vec]
  (reduce (fn [r sv]
            (let [p (reduce * sv)]
              (if (> p (peek r))
                (conj sv p)
                r)))
          [-1]
          (subpartitionv width 1 data-vec)))

(defn e9 []
  (max-prod 13 data-vec))
