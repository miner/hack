(ns eric.minnie)

;; https://gist.github.com/ericnormand/4ca47720a954307739aaeb12682de98a


;; Maxie and Minnie
;; 
;; The maxxie of a number n is the largest number you can achieve by swapping two of its
;; digits (in decimal) (or choosing not to swap if it is already the largest possible). The
;; minnie is the smallest with one swap (though you can't swap a zero digit into the most
;; significant position).
;; 
;; Your task is to write a function that takes an integer and returns a tuple of the maxxie
;; and minnie.  Notes:
;; 
;; Swap any two decimal digits
;; No leading zeroes
;; Don't swap if you can't make it bigger/smaller


;;; refactor search function, submitted later.

;;; trick is that reducing function depends on `from` starting point as special rule about
;;; no leading zeros, which only affects finding min.

(defn swapmaxmin [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        cnt1 (dec (count dv))
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        rfn-max (fn [_] (fn [j i] (if (> (dv i) (dv j)) i j)))
        rfn-min (fn [from] (if (zero? from)
                             (fn [j i] (cond (zero? (dv i)) j
                                             (< (dv i) (dv j)) i
                                             :else j))
                             (fn [j i] (if (< (dv i) (dv j)) i j))))
        search (fn [sfn-from]
                 (loop [from 0]
                   (let [i (reduce (sfn-from from) from (range cnt1 from -1))]
                     (if (= i from)
                       (if (< from cnt1) (recur (inc from)) n)
                       (sumv (assoc dv from (dv i) i (dv from)))))))]
    [(search rfn-max)
     (search rfn-min)]))


;;; originally submitted, but then revised as above
(defn swapmaxmin-orig [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        cnt1 (dec (count dv))
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        rfn-max (fn [j i] (if (> (dv i) (dv j)) i j))
        rfn-min (fn [j i] (if (< (dv i) (dv j)) i j))
        rfn-minz (fn [j i] (cond (zero? (dv i)) j
                                 (< (dv i) (dv j)) i
                                 :else j))]
    [(loop [from 0]
       (let [i (reduce rfn-max from (range cnt1 from -1))]
         (if (= i from)
           (if (< from cnt1) (recur (inc from)) n)
           (sumv (assoc dv from (dv i) i (dv from))))))
     (loop [from 0]      
       (let [i (reduce (if (zero? from) rfn-minz rfn-min) from (range cnt1 from -1))]
         (if (= i from)
           (if (< from cnt1) (recur (inc from)) n)
           (sumv (assoc dv from (dv i) i (dv from))))))]))


;; fastest if you need a vector
(defn digvec [n]
  (mapv (fn [c] (- (long c) (long \0))) (str n)))

;; slow
(defn digvec2 [n]
  (mapv parse-long (map str (str n))))


(defn sumv [dv]
  (reduce (fn [r d] (+ (* r 10) d)) 0 dv))

;;; FIXME -- should be sensitive to ties.  It's better to take from lower max, higher min
;;; probably need to reverse range

(defn mindex [dv]
  (into (sorted-map) (map-indexed (fn [i x] (vector x i)) dv)))

;; slightly faster
(defn tindex [dv]
  (transduce (map-indexed vector)
             (fn ([sm [i x]] (assoc sm x i))
               ([sm] (mapv identity sm)))
             (sorted-map) 
             dv))

;; pretty but slower
(defn sindex [dv]
  (into [] (sort (zipmap dv (range)))))

(defn smindex [dv]
  (into [] (sort (mapv vector dv (range)))))


(defn swapmm [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        dcnt (count dv)
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        indv (tindex dv)
        ;; indv is sorted vector low to high of [dig ind]
        ;; should be less searching
        find-max (fn []
                   (loop [from 0 rinds (rseq indv)]
                     ;;(println "RINDS" rinds)
                     (if (>= from dcnt)
                       dv
                       (if-let [[d i] (first rinds)]
                         ;;(println from d i)
                         (cond (< i from) (recur from (rest rinds))
                               (<= d (dv from)) (recur (inc from) rinds)
                               :else (do ;;(println " MAX" from d i)
                                       (assoc dv from d i (dv from))))
                         dv))))

        find-min (fn []
                   (loop [from 0 inds (seq indv) zi nil]
                     ;;(println "INDS" inds)
                     (if (>= from dcnt)
                       dv
                       (let [[d i] (first inds)]
                         ;;(println from d i)
                         (cond (nil? d) dv
                               (< i from) (recur from (rest inds) zi)
                               (>= d (dv from)) (recur (inc from)
                                                       (if zi (conj inds [0 zi]) inds) nil)
                               (and (zero? from) (zero? d)) (recur from (rest inds) i)
                               :else (do ;;(println " MIN" from d i)
                                         (assoc dv from d i (dv from)))))))) ]
    ;;(println "INDV" indv)
    [(sumv (find-max))
     (sumv (find-min))]))






(defn smoke-swap [swapmaxmin]
  (assert (= (swapmaxmin 213)  [312, 123]))
  (assert (= (swapmaxmin 12345) [52341, 12345]))
  ;; the number was already the smallest
  (assert (= (swapmaxmin 100) [100, 100]))
  ;; no swap possible because of zeroes
  (assert (= (swapmaxmin 1909) [9901 1099]))
  (assert (= (swapmaxmin 19090) [99010 10099]))
  (assert (= (swapmaxmin 92484957) [99484257 29484957]))
  (assert (= (swapmaxmin 120142185) [820142115 102142185]))
  (assert (= (swapmaxmin 5555555555555604) [6555555555555504 4555555555555605]))
  true)





;; digits in reverse order
(defn rdigs [n]
  (loop [n n ds []]
    (if (zero? n)
      ds
      (recur (quot n 10) (conj ds (rem n 10))))))

(defn slow-idigs [ds]
  (parse-long (apply str ds)))


;; maybe faster for seqs, but not much
#_
(defn digs [n]
  (loop [n n ds nil]
    (if (zero? n)
      ds
      (recur (quot n 10) (conj ds (rem n 10))))))

(defn rndigs [dv]
  (reduce (fn [r d] (+ (* r 10) d)) 0 (rseq dv)))

;; assume same size ds es
(defn digs> [ds es]
  (when-let [d (first ds)]
    (if (= d (first es))
      (recur (rest ds) (rest es))
      (> d (first es)))))

(defn digs< [ds es]
  (when-let [d (first ds)]
    (if (= d (first es))
      (recur (rest ds) (rest es))
      (< d (first es)))))

;; slower
(defn ds> [ds es]
  (pos? (first (remove zero? (map compare ds es)))))



;; ----------------------------------------------------------------------


;; slow @sw
(defn sw-swaps [n]
  (let [s (vec (str n))]
    (for [j     (range 1 (count s))
          i     (range j)
          :let  [s' (assoc s i (nth s j) j (nth s i))]
          :when (not= \0 (nth s' 0))]
      (read-string (apply str s')))))

(defn sw-swapmaxmin [n]
  ((juxt #(reduce max n %) #(reduce min n %)) (sw-swaps n)))



;;; save mine
(defn swapmaxmin1 [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        find-max (fn [dv from]
                   (if (< from (count dv))
                     (let [i (reduce (fn [j i] (if (> (dv i) (dv j)) i j))
                                     from
                                     (range (dec (count dv)) from -1))]
                       (if (= i from)
                         (recur dv (inc from))
                         (assoc dv from (dv i) i (dv from))))
                     dv))
        find-min (fn [dv from]      
                   (let [test? (if (zero? from) #(pos? (dv %)) identity)]
                     (if (< from (count dv))
                       (let [i (reduce (fn [j i] (if (and (test? i) (< (dv i) (dv j))) i j))
                                       from
                                       (range (dec (count dv)) from -1))]
                         (if (= i from)
                           (recur dv (inc from))
                           (assoc dv from (dv i) i (dv from))))
                       dv)))]
    [(sumv (find-max dv 0))
     (sumv (find-min dv 0))]))



;; elevate the lowest indexed high digit

(defn find-max [dv from]
  (if (< from (count dv))
    (let [i (reduce (fn [j i] (if (> (dv i) (dv j)) i j))
                    from
                    (range (dec (count dv)) from -1))]
      (if (= i from)
        (recur dv (inc from))
        (assoc dv from (dv i) i (dv from))))
    dv))
      

;; elevate the lowest indexed low dig
(defn find-min [dv from]      
  (let [test? (if (zero? from) #(pos? (dv %)) identity)]
    (if (< from (count dv))
      (let [i (reduce (fn [j i] (if (and (test? i) (< (dv i) (dv j))) i j))
                      from
                      (range (dec (count dv)) from -1))]
        (if (= i from)
          (recur dv (inc from))
          (assoc dv from (dv i) i (dv from))))
      dv)))
;; elevate the lowest indexed low dig



;;; save
(defn swapmaxmin2 [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        dcnt (count dv)
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        find-max (fn [from]
                   (if (< from dcnt)
                     (let [i (reduce (fn [j i] (if (> (dv i) (dv j)) i j))
                                     from
                                     (range (dec dcnt) from -1))]
                       (if (= i from)
                         (recur (inc from))
                         (assoc dv from (dv i) i (dv from))))
                     dv))
        find-min (fn [from]      
                   (let [test? (if (zero? from) #(pos? (dv %)) identity)]
                     (if (< from dcnt)
                       (let [i (reduce (fn [j i] (if (and (test? i) (< (dv i) (dv j))) i j))
                                       from
                                       (range (dec dcnt) from -1))]
                         (if (= i from)
                           (recur (inc from))
                           (assoc dv from (dv i) i (dv from))))
                       dv)))]
    [(sumv (find-max 0))
     (sumv (find-min 0))]))

;; save
(defn swapmaxmin3 [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        dcnt (count dv)
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        find-max (fn [from]
                   (if (< from dcnt)
                     (let [i (reduce (fn [j i] (if (> (dv i) (dv j)) i j))
                                     from
                                     (range (dec dcnt) from -1))]
                       (if (= i from)
                         (recur (inc from))
                         (assoc dv from (dv i) i (dv from))))
                     dv))
        find-min (fn [from]      
                     (if (< from dcnt)
                       (let [i (reduce (fn [j i]
                                         (cond (and (zero? from) (zero? (dv i))) j
                                               (< (dv i) (dv j)) i
                                               :else j))
                                       from
                                       (range (dec dcnt) from -1))]
                         (if (= i from)
                           (recur (inc from))
                           (assoc dv from (dv i) i (dv from))))
                       dv))]
    [(sumv (find-max 0))
     (sumv (find-min 0))]))


(defn swapmaxmin4 [n]
  (let [dv (mapv (fn [c] (- (long c) (long \0))) (str n))
        dcnt (count dv)
        sumv (fn [dv] (reduce (fn [r d] (+ (* r 10) d)) 0 dv))
        find-max #(loop [from 0]
                    (if (>= from dcnt)
                      dv
                      (let [i (reduce (fn [j i] (if (> (dv i) (dv j)) i j))
                                      from
                                      (range (dec dcnt) from -1))]
                        (if (= i from)
                          (recur (inc from))
                          (assoc dv from (dv i) i (dv from))))))
        find-min #(loop [from 0]      
                    (if (>= from dcnt)
                      dv
                      (let [i (reduce (fn [j i]
                                        (cond (and (zero? from) (zero? (dv i))) j
                                              (< (dv i) (dv j)) i
                                              :else j))
                                      from
                                      (range (dec dcnt) from -1))]
                        (if (= i from)
                          (recur (inc from))
                          (assoc dv from (dv i) i (dv from))))))]
    [(sumv (find-max))
     (sumv (find-min))]))
