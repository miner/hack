(ns miner.enewnum)

;;; https://gist.github.com/ericnormand/fb8d0356a5ff7d898707012a97975ec7

;;; New Numbers
;;;
;;; A number is new if its digits are not a permutation of a smaller number. For instance,
;;; 789 is a new number because its permutations (879, 798, 897, 978, and 987) are all
;;; larger than it is. However, 645 is not a new number since 456 and 465 are smaller than
;;; it.  Bonus: You may find a clever way to write new-number?. In addition to that
;;; implementation, implement it in such a way that the definition (no permutations are
;;; smaller) is clear from the code.

;;; Edabit clarification: 509 is a new number because it can't be formed by a permutation of
;;; any smaller number (leading zeros not allowed).

;;; Definitive reference:  https://oeis.org/A179239
;;; "Permutation classes of integers, each identified by its smallest member"
;;; has note about no leading zeros allowed in permutations

;;; Notice: 0...20 are all "new-numbers" so you can maybe simplify some things

;; corrected for second 0 and maybe more
(defn new-number? [n]
  (let [digs (seq (str n))]
    (boolean (reduce (fn [a b] (if (pos? (compare a b)) (reduced false) b))
                     (first digs)
                     (drop-while #{\0} (rest digs))))))

(defn xnew-number? [n]
  (let [digs (seq (str n))]
    (boolean (reduce (fn [a b] (if (pos? (compare a b)) (reduced false) b))
                     (first digs)
                     (drop-while #(identical? \0 %) (rest digs))))))

;; a bit faster with identical? test, transduce doesn't seem to matter
(defn tnn? [n]
  (let [digs (seq (str n))]
    (transduce (drop-while #(identical? \0 %))
               (completing (fn [a b] (if (pos? (compare a b)) (reduced false) b))
                           boolean)
               (first digs)
               (rest digs))))

                                              
;; corrected but slow
(defn nnum? [n]
  (let [digs (seq (str n))
        digs (if (= (second digs) \0)
               (conj (drop-while #{\0} (rest digs)) (first digs))
               digs)]
    (= digs (sort digs))))

;;; slightly faster with identical? instead of =
(defn nnum2? [n]
  (let [digs (seq (str n))
        digs (if (identical? (second digs) \0)
               (conj (drop-while #(identical? \0 %) (rest digs)) (first digs))
               digs)]
    (= digs (sort digs))))



;; original is fast but flawed with trailing zeroes
(defn new-number-SUBMITTED? [n]
  (boolean (reduce (fn [a b] (if (pos? (compare a b)) (reduced false) b)) \0 (str n))))
;; but doesn't handle 0 chars correctly

;; zero problem
(defn nn1? [n]
  (<= n (Long/parseLong (apply str (sort (str n))))))



(defn smoke-nn [new-number?]
  (assert (new-number? 789))
  (assert (not (new-number? 645)))
  (assert (new-number? 444))
  (assert (new-number? 123456789))
  (assert (new-number? 30))
  (assert (new-number? 509))
  (assert (new-number? 100))
  (assert (new-number? 1000))
  (assert (new-number? 1001))
  (assert (not (new-number? 2001)))
  (assert (not (new-number? 5109)))
  (assert (not (new-number? 123456780)))
  (assert (= (count (filter new-number? (range 1000000))) 8002))
  true)


;; @mchampine  fails 1230
(defn mch? [n]
  (let [nzdigits (remove #{\0} (seq (str n)))
        nzsdigs (remove #{\0} (sort nzdigits))]
    (= nzdigits nzsdigs)))

;; @mcuervoe fails 30
(defn mcu? [n]
  (let [nstr (str n)]
    (every? (fn [[a b]] (<= (int a) (int b))) (map (fn [a b] [a b]) nstr (drop 1 nstr)))))

;; @steffan-westcott, hacked by SEM for 0, still much slower than mine
(defn swnn? [n]
  (or (zero? n)
      (let [digits (seq (str n))
            [zeroes [non-zero & rest-non-zeroes]] (split-with #{\0} (sort digits))
            min-perm (concat [non-zero] zeroes rest-non-zeroes)]
        (= min-perm digits))))


;;; @mchampine, hacked by SEM for namespaces, then hacked again to work for 0.  Very slow!
;;; Can't time for 1e6 count
(require 'clojure.math.combinatorics)
(defn mch2? [n]
  (->> (clojure.math.combinatorics/permutations (str n))
       (remove #(identical? \0 (first %)))
       (map clojure.string/join)
       (map #(Integer/parseInt %))
       (apply min n)
       (= n)))

;; @safehammad   hacked by SEM to handle 0 -- actually all <= 20
(defn sann? [n]
  (or (<= n 20)
  (let [ascii (map int (str n))]
    (and
      (apply <= 0 (rest ascii))
      (apply <= (remove #{(int \0)} ascii))))))


(defn digits [n]
  (loop [r n ds nil]
    (if (< r 10)
      (conj ds r)
      (recur (quot r 10) (conj ds (rem r 10))))))



;; @jpmonettas 
(defn jpnn? [^long n]
  ;; moving from the back digits should be always decreasing
  ;; always but skipping zeroes
  (loop [rem-digits n 
         prev 9 
         last-non-zero 9]
    (let [d (rem rem-digits 10)
          nextd (quot rem-digits 10)
          more? (pos? nextd)]
      (if (<= d prev)
        ;; when decreasing continue unles we reached the end
        (if more?
          
          (recur nextd d (if (zero? prev) last-non-zero prev))
          
          true) ;; done, the number looks new

        ;; else not decreasing check decreasing over the zeroes bridge
        (and (not more?)
             (zero? prev)
             (<= d last-non-zero))))))




;; my version of jp.  With a slightly different take on zeroes.  Working from
;; least-significant digit up, once I see a zero, we lock in to requiring zeroes, until the
;; q is the final
;; (first!) digit.  Note p (previous) is always non-zero.
(defn fastnn? [^long n]
  (loop [q n p 9 z? false]
    (if (< q 10)
      (<= q p)
      (let [d (rem q 10)
            zd? (zero? d)]
        (cond z? (and zd? (recur (quot q 10) p true))
              zd? (recur (quot q 10) p true)
              (<= d p) (recur (quot q 10) d false)
              :else false)))))

