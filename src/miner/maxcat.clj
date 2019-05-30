(ns miner.maxcat)

;;; SEM -- my basic idea is WRONG for sorting.  Beware 121 vs 1.  See v9c for fix.


;; need to add test
;;  (is (= 1211 (sut/maxcat [121 1]) (sut/maxcat [1 121])))


;; My version, based on Valentin Waeselynck's solution.
;; https://gist.github.com/ericnormand/3ebf88205b58b31252b901888622d13d#file-valentin-waeselynck-clj

(defn maxcat
   "Returns the largest BigInt you can create by concatenating
  the non-negative long integers in ns."
  [ns]
  (let [dcompare (fn [a b]
                   (let [c (compare (str b a) (str a b))]
                     (if (zero? c)
                       (compare (count a) (count b))
                       c)))]
    (bigint (apply str (sort dcompare (map str ns))))))


;; not worth trying to avoid str consing
(defn maxcat2
   "Returns the largest BigInt you can create by concatenating
  the non-negative long integers in ns."
  [ns]
  (let [dcompare (fn [as bs]
                   (let [cnta (count as)
                         cntb (count bs)
                         total (+ cnta cntb)]
                     (loop [i 0]
                       (if (= i total)
                         (compare cnta cntb)
                         (let [a (get as i (get bs (- i cnta)))
                               b (get bs i (get as (- i cntb)))
                               c (compare b a)]
                           (if (zero? c)
                             (recur (inc i))
                             c))))))]
    (bigint (apply str (sort dcompare (map str ns))))))




;; hints don't do much, better to be pure Clojure
(defn maxcat1
   "Returns the largest integer you can create by concatenating
  the non-negative integers in ns."
  [ns]
  (let [dcompare (fn [^String s1 ^String s2]
                   (let [c (compare (str s2 s1) (str s1 s2))]
                     (if (zero? c)
                       (compare (.length s1) (.length s2))
                       c)))]
    (bigint (apply str (sort dcompare (map str ns))))))



(defn maxcatWRONG [ns]
  (let [d9compare (fn [^String astr ^String bstr]
                    (if (= (.length astr) (.length bstr))
                      (compare bstr astr) ;bstr first to get desired order
                      (loop [i 0]
                        (cond (= i (.length astr)) -1
                              (= i (.length bstr)) 1
                              :else (let [c (compare (.charAt bstr i) (.charAt astr i))]
                                      (if (zero? c)
                                        (recur (inc i))
                                        c))))))]
    (bigint (apply str (sort d9compare (map str ns))))))


;; proctor WRONG
(defn proctor [l] (->> l (sort-by str) reverse (clojure.string/join "") bigint))


;; Valentin is susceptible to very large ints [Long/MAX_VALUE Long/MAX_VALUE] will break it.
;; I think it's safer and almost as quick to just use the strings without parseLong.

;; (Mostly) Correct and faster
(defn valentin [is]
  (->> is
    (mapv #(Long/toString % 10))
    (sort (fn compare-base10 [s1 s2]
            (compare
              (Long/parseLong (str s2 s1) 10)
              (Long/parseLong (str s1 s2) 10))))
    (apply str)
    (read-string)))



;; extracted from valentin.  Correct but not consistent on 5 vs 555.
(defn vcompare [s1 s2]
  (compare (Long/parseLong (str s2 s1) 10)
           (Long/parseLong (str s1 s2) 10)))

;; prefer shorter when otherwise equal
(defn v9c1 [^String s1 ^String s2]
  (let [c (compare (Long/parseLong (str s2 s1) 10)
                   (Long/parseLong (str s1 s2) 10))]
    (if (zero? c)
      (compare (.length s1) (.length s2))
      c)))

;; stable, slightly slower
(defn v9c [^String s1 ^String s2]
  (let [c (compare (str s2 s1) (str s1 s2))]
    (if (zero? c)
      (compare (.length s1) (.length s2))
      c)))


(defn mc-test []
  (let [nnn [9 99 91 9997 998 9 1 10 121 11]
        onetwo [1 12 10]
        bigs [1234567890 1919191919]]
    (assert (= (valentin nnn) (maxcat nnn)))
    (assert (= (valentin onetwo) (maxcat onetwo)))
    (assert (= (maxcat bigs) (bigint (apply str (rseq bigs)))))
    (print "valentin ")
    (time (valentin nnn))
    (print "maxcat ")
    (time (maxcat nnn))))

