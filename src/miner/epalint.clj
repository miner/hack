(ns miner.epalint)

;; https://gist.github.com/ericnormand/24d5144ffa84c3db94de542cebd5b37c

;; An integer is a palindrome if it is written with the same decimal digits forward and
;; backward. Your job is to write a function that takes an integer and returns the closes
;; palindrome integer to it. The closest one could be larger or smaller than the given
;; integer.  (return the smaller in case of tie)


;; same as clopal4, ugly but pretty fast
(defn closest-palindrome [n]
  (let [abs (fn [x] (if (neg? x) (- x) x))
        guess (fn [n]
                (let [s (str n)
                      sb (StringBuilder. s)
                      hlen (quot (.length sb) 2)]
                  (Long/parseLong (-> sb .reverse
                                      (.replace 0 hlen (subs s 0 hlen)) .toString))))]
      (let [g (guess n)
            diff (- g n)]
        (if (zero? diff)
          n
          (let [g2 (guess (- n diff))
                ad (abs diff)
                ad2 (abs (- g2 n))]
            (if (= ad ad2)
              (min g g2)
              (if (< ad ad2) g g2)))))))




;; brute force
(defn brute-closest-palindrome [n]
  (let [palin? (fn [n]
                 (let [s (str n)]
                   (= s (-> (StringBuilder. s) .reverse .toString))))]
    (if (palin? n)
      n
      (loop [lo (dec n) hi (inc n)]
        (cond (palin? lo) lo
              (palin? hi) hi
              :else (recur (dec lo) (inc hi)))))))


;; slow
(defn xcp1 [n]
  (let [palin? (fn [n]
                 (let [s (str n)]
                   (= s (-> (StringBuilder. s) .reverse .toString))))]
    (if (palin? n)
      n
      (first (sequence (comp (mapcat (juxt - +))
                             (filter palin?)
                             (take 1))
                       (repeat n)
                       (range 1 n))))))


(defn xcp2 [n]
  (let [palin? (fn [n]
                 (let [s (str n)]
                   (= s (-> (StringBuilder. s) .reverse .toString))))]
    (if (palin? n)
      n
      (first (sequence (comp (mapcat (juxt #(- n %) #(+ n %)))
                             (filter palin?)
                             (take 1))
                       (range 1 n))))))


(defn xcp3 [n]
  (let [palin? (fn [n]
                 (let [s (str n)]
                   (= s (-> (StringBuilder. s) .reverse .toString))))]
    (if (palin? n)
      n
      (first (sequence (comp (mapcat #(vector (- n %) (+ n %)))
                             (filter palin?)
                             (take 1))
                       (range 1 n))))))



;; construct candidates, but doesn't consider lower first digit

;; hack
(defn clopal [n]
  (let [palin? (fn [n]
                 (let [s (str n)]
                   (= s (-> (StringBuilder. s) .reverse .toString))))
        rev (fn [^String s] (-> (StringBuilder. s) .reverse .toString))
        guess (fn [n]
                (let [s (str n)
                      cnt (count s)
                      half (subs s 0 (quot (inc cnt) 2))
                      guess (if (odd? cnt) (str half (subs (rev half) 1)) (str half (rev half)))]
                  (Long/parseLong guess)))
        ndiff (fn [x] (let [d (- n x)] (if (neg? d) (- d) d)))]
    (if (palin? n)
      n
      (let [g (guess n)
            diff (- g n)
            g2 (guess (- n diff))
            diff2 (- g2 n)]
        (if (< g2 g)
          (min-key ndiff g g2)
          (min-key ndiff g2 g))))))

;; BUT we don't really need palin? above.  If it's accidentally pal, it will work with
;; the flip.



(defn clopal2 [n]
  (let [rev (fn [^String s] (-> (StringBuilder. s) .reverse .toString))
        guess (fn [n]
                (let [s (str n)
                      cnt (count s)
                      half (subs s 0 (quot (inc cnt) 2))
                      guess (if (odd? cnt) (str half (subs (rev half) 1)) (str half (rev half)))]
                  (Long/parseLong guess)))]
      (let [g (guess n)
            diff (- g n)]
        (if (zero? diff)
          n
          (let [g2 (guess (- n diff))
                ndiff (fn [x] (let [d (- n x)] (if (neg? d) (- d) d)))]
            (if (< g2 g)
              (min-key ndiff g g2)
              (min-key ndiff g2 g)))))))


;; pretty fast
(defn clopal3 [n]
  (let [guess (fn [n]
                (let [s (str n)
                      sb (StringBuilder. s)
                      hlen (quot (.length sb) 2)]
                  (Long/parseLong (-> sb .reverse
                                      (.replace 0 hlen (subs s 0 hlen)) .toString))))]
      (let [g (guess n)
            diff (- g n)]
        (if (zero? diff)
          n
          (let [g2 (guess (- n diff))
                ndiff (fn [x] (let [d (- n x)] (if (neg? d) (- d) d)))]
            (if (< g2 g)
              (min-key ndiff g g2)
              (min-key ndiff g2 g)))))))


;; fastest but a bit ugly
(defn clopal4 [n]
  (let [abs (fn [x] (if (neg? x) (- x) x))
        guess (fn [n]
                (let [s (str n)
                      sb (StringBuilder. s)
                      hlen (quot (.length sb) 2)]
                  (Long/parseLong (-> sb .reverse
                                      (.replace 0 hlen (subs s 0 hlen)) .toString))))]
      (let [g (guess n)
            diff (- g n)]
        (if (zero? diff)
          n
          (let [g2 (guess (- n diff))
                ad (abs diff)
                ad2 (abs (- g2 n))]
            (if (= ad ad2)
              (min g g2)
              (if (< ad ad2) g g2)))))))


;; 2x - 10x slower with pure Clojure
(defn clopal41 [n]
  (let [abs (fn [x] (if (neg? x) (- x) x))
        guess (fn [n]
                (let [s (str n)
                      rs (clojure.string/reverse s)
                      hlen (quot (count s) 2)]
                  ;;(println s rs hlen (subs s 0 hlen) (subs rs hlen))
                  (clojure.edn/read-string (str (subs s 0 hlen) (subs rs hlen)))))]
      (let [g (guess n)
            diff (- g n)]
        (if (zero? diff)
          n
          (let [g2 (guess (- n diff))
                ad (abs diff)
                ad2 (abs (- g2 n))]
            (if (= ad ad2)
              (min g g2)
              (if (< ad ad2)
                g
                g2)))))))






(defn clopal5 [n]
  (let [absdiff (fn [g] (let [d (- n g)] (if (neg? d) (- d) d)))
        guess (fn [n]
                (let [s (str n)
                      sb (StringBuilder. s)
                      hlen (quot (.length sb) 2)]
                  (Long/parseLong (-> sb .reverse
                                      (.replace 0 hlen (subs s 0 hlen)) .toString))))]
      (let [g (guess n)
            diff (absdiff g)]
        (if (zero? diff)
          n
          (let [g2 (if (> g n) (guess (- n diff)) (guess (+ n diff)))
                diff2 (absdiff g2)]
            (if (= diff diff2)
              (min g g2)
              (if (< diff diff2)
                g
                g2)))))))


;; not actually faster than 5
(defn clopal6 [n]
  (let [absdiff (fn [g] (let [d (- n g)] (if (neg? d) (- d) d)))
        guess (fn [n]
                (let [s (str n)
                      sb (StringBuilder. s)
                      hlen (quot (.length sb) 2)]
                  (Long/parseLong (-> sb .reverse (.replace 0 hlen (subs s 0 hlen)) .toString))))]
      (let [g (guess n)
            diff (absdiff g)]
        (if (zero? diff)
          n
          (if (> g n)
            (let [g2 (guess (- n diff))
                  diff2 (absdiff g2)]
              (if (< diff diff2)
                g
                g2))
            (let [g2 (guess (+ n diff))
                  diff2 (absdiff g2)]
              (if (< diff2 diff)
                g2
                g)))))))

;; about 2x slower avoiding Java interop
(defn clopal61 [n]
  (let [absdiff (fn [g] (let [d (- n g)] (if (neg? d) (- d) d)))
        guess (fn [n]
                (let [s (str n)
                      len (count s)
                      hlen (quot len 2)
                      hstr (subs s 0 hlen)
                      rhstr (clojure.string/reverse hstr)]
                  ;;(println s hstr rhstr)
                  (if (odd? len)
                    (Long/parseLong (str (subs s 0 (inc hlen)) rhstr))
                    (Long/parseLong (str hstr rhstr)))))]
      (let [g (guess n)
            diff (absdiff g)]
        (if (zero? diff)
          n
          (if (> g n)
            (let [g2 (guess (- n diff))
                  diff2 (absdiff g2)]
              (if (< diff diff2)
                g
                g2))
            (let [g2 (guess (+ n diff))
                  diff2 (absdiff g2)]
              (if (< diff2 diff)
                g2
                g)))))))










;; 10x slower with vector chars!
(defn clopal7 [n]
  (let [absdiff (fn [g] (let [d (- n g)] (if (neg? d) (- d) d)))
        guess (fn [n]
                (let [dv (vec (seq (str n)))
                      cnt (count dv)
                      hcnt (quot (inc cnt) 2)]
                  (->> (into (subvec dv 0 hcnt)
                             (drop hcnt)
                             (rseq dv))
                       (apply str)
                       Long/parseLong)))]
    (let [g (guess n)
          diff (absdiff g)]
      (if (zero? diff)
        n
        (if (> g n)
          (let [g2 (guess (- n diff))
                diff2 (absdiff g2)]
            (if (< diff diff2)
              g
              g2))
          (let [g2 (guess (+ n diff))
                diff2 (absdiff g2)]
            (if (< diff2 diff)
              g2
              g)))))))



    
(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))


(defn smoke-pal [closest-palindrome]
  (assert? = (closest-palindrome 100) 99
           (closest-palindrome 100000) 99999
           (closest-palindrome 12880) 12921
           (closest-palindrome 128980) 128821
           (closest-palindrome 128180) 127721
           (closest-palindrome 80) 77
           (closest-palindrome 6789) 6776
           (closest-palindrome 887)  888
           (closest-palindrome 888) 888))



;; @steffan-westcott, refactored by SEM

(defn sw-closest-palindrome [n]
  (let [abs (fn [n] (if (neg? n) (- n) n))
        closest-smallest-integer (fn [n xs] (apply min-key #(abs (- n % 0.1)) xs))]
    (if (< n 10)
      n
      (let [s (str n)
            low-digits-length (quot (count s) 2)
            n-high-digits (read-string (subs s 0 (- (count s) low-digits-length)))
            palindromes (for [high (take 3 (iterate inc (dec n-high-digits)))
                              :let [high' (if (zero? high) "" (str high))
                                    low' (clojure.string/reverse (subs (str high' "9") 0 low-digits-length))]]
                          (read-string (str high' low')))]
        (closest-smallest-integer n palindromes)))))
