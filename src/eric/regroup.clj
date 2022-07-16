(ns eric.regroup
  (:require [clojure.string :as str]))


;; https://gist.github.com/ericnormand/415c98b46e216978728156fecc20bac4

;;; Write a function that takes a license plate code (letters, digits, and hyphens in a
;;; string) and a group size (integer). The function should return a new string with the
;;; characters regrouped with hyphens between groups. All groups should be of the given
;;; size, except for perhaps the first, if there aren't enough characters to fill the group.

;; faster to use interop for String insertions
(defn regroup [s width]
  (let [base (str/replace s "-" "")
        cnt (count base)]
    (str (reduce (fn [sb i] (.insert ^StringBuilder sb ^int i \-))
                 (StringBuilder. base)
                 (range (- cnt width) 0 (- width))))))

;; not submitted but a little faster
(defn regroup-fastest [s width]
  (let [base (.replace ^String s "-" "")]
    (str (reduce (fn [sb i] (.insert ^StringBuilder sb ^int i \-))
                 (StringBuilder. base)
                 (range (- (.length base) width) 0 (- width))))))


(defn reg1 [s width]
  (let [letters (into [] (remove #(= % \-)) s)
        cnt (count letters)
        extra (rem cnt width)]
    (str/join  "-" (into (if (zero? extra) [] [(str/join (take extra letters))])
                         (comp (drop extra) (partition-all width) (map str/join))
                         letters))))

;; faster 2.8 us
(defn reg2 [s width]
  (let [base (str/replace s "-" "")
        cnt (count base)
        extra (rem cnt width)]
    (str/join  "-" (into (if (zero? extra) [] [(str/join (take extra base))])
                         (comp (drop extra) (partition-all width) (map str/join))
                         base))))
  
;; faster, 500 us
(defn reg4 [s width]
  (let [base (str/replace s "-" "")
        cnt (count base)]
    (loop [i (- cnt width) sb (StringBuilder. ^String base)]
      (if (pos? i)
        (recur (- i width) (.insert sb ^int i \-))
        (str sb)))))

;;; faster (slightly), 420 ns
(defn reg5 [s width]
  (let [base (.replace ^String s "-" "")
        cnt (count base)]
    (loop [i (- cnt width) sb (StringBuilder. ^String base)]
      (if (pos? i)
        (recur (- i width) (.insert sb ^int i \-))
        (str sb)))))


;; fastest?   388 ns
(defn reg6 [s width]
  (let [base (.replace ^String s "-" "")
        cnt (count base)]
    (str (reduce (fn [sb i] (.insert ^StringBuilder sb ^int i \-))
                 (StringBuilder. base)
                 (range (- cnt width) 0 (- width))))))





(defn smoke-reg [regroup]
  (assert (= (regroup "A5-GG-B88" 3) "A-5GG-B88"))
  (assert (= (regroup "A5-GG-B88" 2) "A-5G-GB-88"))
  (assert (= (regroup "6776" 2) "67-76"))
  (assert (= (regroup "F33" 1) "F-3-3"))
  (assert (= (regroup "IIO" 7) "IIO"))
  true)




;;; ----------------------------------------------------------------------




;; not faster
(defn rangev [start end step]
  (let [cont? (if (neg? step) > <)]
    (loop [res [] i start]
      (if (cont? i end)
        (recur (conj res i) (+ i step))
        res))))



(defn regroupOK [s width]
  (let [letters (into [] (remove #{\-}) s)
        cnt (count letters)
        extra (rem cnt width)
        start (if (zero? extra) [] [(str/join (take extra letters))])]
    (str/join  "-" (into start
                         (comp (drop extra) (partition-all width) (map str/join))
                         letters))))



;; still slower
(defn reg61x [s width]
  (let [base (.replace ^String s "-" "")
        cnt (count base)]
    (str (reduce (fn [sb i] (.insert ^StringBuilder sb ^int i \-))
                 (StringBuilder. ^String base)
                 (take-while pos? (iterate #(- % width) (- cnt width)))))))

;; slow 628 us
(defn reg62x [s width]
  (let [base (.replace ^String s "-" "")
        cnt (count base)]
    (transduce (take-while pos?)
               (fn ([sb i] (.insert ^StringBuilder sb ^int i \-))
                 ([sb] (str sb)))
               (StringBuilder. ^String base)
               (iterate #(- % width) (- cnt width)))))



;;; not faster
(defn reg3 [s width]
  (let [base (str/replace s "-" "")
        cnt (count base)
        extra (rem cnt width)]
    (transduce  (comp (drop extra) (partition-all width) (interpose  [\-]) cat)
                (completing conj str/join)
                (cond-> []
                  (pos? extra) (into (take extra base))
                  (and (pos? extra) (< extra cnt)) (conj \-))
                base)))

;;; @sw1nn
(defn sw-regroup [s n]
  (let [patt (re-pattern (str "(?:^.{0," n "}$|^.{" n "}(?!..{" n ",})|^.{1," n "}?|(?<!^).{" n "}+)"))]
    (->> (str/replace s "-" "")
         (re-seq patt)
         (str/join "-"))))

;;; @mchampine  very slow
(defn mc-regroup [s n]
  (->> (remove #{\-} (reverse s))
       (partition-all n)
       (map #(apply str (reverse %)))
       reverse
       (str/join "-")))


;;; SEM hack to speed up mc, but still pretty slow
(defn sm-mc-reg [s n]
  (->> (remove #(= % \-) (.reverse (StringBuilder. ^String s)))
       (partitionv-all n)
       (mapv #(apply str (rseq %)))
       rseq
       (str/join "-")))
