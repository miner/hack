(ns miner.moving)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-363-learn-to-build-and-deploy-a-single-page-application/


;; Open question about dropping first couple of averages to get a "central" window and sync
;; up the (time) order.  Or just pre-populate with the start and finish numbers to get a
;; full count of moving average.

;; Moving average
(defn simple-moving-average [n coll]
  (->> coll
       (partition n 1)
       (map #(/ (reduce + %) n))))


;; correcting for start-up lag by taking initial half window literally
(defn moving-average [n coll]
  (let [pre (quot (dec n) 2)
        wv (->> coll
                (partition n 1)
                (mapv #(/ (reduce + %) n)))]
    (concat (take pre coll)
            wv
            (drop (+ (count wv) pre) coll))))



;; don't like having to use count.  Can't have infinite coll, not good with lazy results
(defn ma2 [n coll]
  (take (count coll)
        (map #(/ (reduce + 0 %) (count %))
             (concat (map #(take % coll) (range 1 (quot (dec n) 2)))
                     (partition-all n 1 coll)))))

;; SEM, not sure about using this, but I like the general idea
(defn elongate [coll n]
  (if (seq (rest coll))
    (lazy-seq (cons (first coll) (elongate (rest coll) n)))
    (if (seq coll)
      (repeat (inc n) (first coll))
      (repeat n nil))))


;; but not really balanced between start up and ending
(defn ma3 [n coll]
  (let [pre (quot n 2)
        post (- n pre)]
    (drop pre
          (map #(/ (reduce + %) n)
               (partition n 1 (concat (take pre coll) (elongate coll post)))))))


;; not same count in result as in original coll
(defn mvgavg [n coll]
  ;; sequence for lazy
  (into [] (map #(/ (reduce + %) n)) (partition n 1 coll)))





(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; reductions idea from Nazarii Bardiuk, but with my spin on it
;; Trying to be balanced with start up and ending partial windows.  Also careful about edge
;; cases with empty or short coll vs wide window, etc.
(defn ma7 [n coll]
  (let [n2 (quot (inc n) 2)
        pre (take n2 coll)]
    (if (< (count pre) n2)
      pre
      (map #(/ (reduce + %) (count %))
           (reductions (fn [w x] (cond (nil? x) (pop w)
                                       (< (count w) n) (conj w x)
                                       :else  (conj (pop w) x)))
                       (queue pre)
                       (concat (drop n2 coll) (repeat (dec n2) nil)))))))



;; if you don't like to see ratios printed you can add (map ratio) to the above funtion
(defn deratio [rat]
  (if (ratio? rat)
    (double rat)
    rat))

(defn deration-moving-average [n coll]
  (map deratio (moving-average n coll)))

  

(def xyz (mapcat #(repeat 3 %) (range 100)))

(defn smoke-avg
  ([] (smoke-avg moving-average))
  ([moving]
   (assert (= (moving 1 (range 100)) (range 100)))
   (assert (= (moving 5 (range 100)) (range 100)))
   (assert (= (moving 4 (interleave (repeat 10 10) (repeat 10 12)))
              '(10 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 10 12)))
   true))





;; not so pretty
(defn moving-average1 [n coll]
  (let [pre (quot (dec n) 2)
        cnt (count coll)]
    (concat (take pre coll)
            (->> coll
                 (partition n 1)
                 (map #(/ (reduce + %) n)))
            (drop (- cnt pre (if (odd? (+ cnt pre)) 1 0)) coll))))

;; too much bias to the beginning, not enough weight at end
(defn ma5 [n coll]
  (map #(/ (reduce + %) (count %))
       (rest (reductions (fn [w x] (conj (if (< (count w) n) w (pop w)) x))
                         (queue)
                         coll))))

;; elongate is maybe weird and counterintuitive
(defn ma6 [n coll]
  (when (seq coll)
    (let [n2 (quot (inc n) 2)]
      (map #(/ (reduce + %) (count %))
           (reductions (fn [w x] (conj (if (< (count w) n) w (pop w)) x))
                       (queue (take n2 coll))
                       (elongate (drop n2 coll) (dec n2)))))))



;; ----------------------------------------------------------------------
;; https://gist.github.com/ericnormand/5a29561f7e220879210c3726ad64e2de
;; Eric's solution

(defn window
  "Return a window of size n, centered on i, of the values (a vector)"
  [n values i]
  (let [n2 (quot n 2)
        n2o (- n n2)
        start (max 0 (- i n2o))
        end (min (count values) (+ i n2))]
   (subvec values start end)))

(defn sum [numbers]
  (reduce + 0 numbers))

(defn avg [numbers]
  (/ (sum numbers) (count numbers)))

(defn eric-moving-average [size numbers]
  (->> numbers
       vec
       (map-indexed #(window size numbers %2))
       (map avg)
       (map double)))


;; SEM corrections

(defn fix-window
  "Return a window of size n, centered on i, of the values (a vector)"
  [n values i]
  (let [offset (quot (inc n) 2)
        end (min (count values) (+ i offset))
        start (max 0 (- i (- n offset)))]
   (subvec values start end)))

(defn sum [numbers]
  (reduce + 0 numbers))

(defn avg [numbers]
  (/ (sum numbers) (count numbers)))

(defn fix-moving-average [size numbers]
  (let [numbers (vec numbers)]
    (->> numbers
         (map-indexed (fn [i _] (fix-window size numbers i)))
         (map avg)
         (map double))))






;; Nazarii Bardiuk.clj

(defn slide [window x]
  (-> window (subvec 1) (conj x)))

(defn windows [n xs]
  (let [first-window (vec (take n xs))
        the-rest (drop n xs)]
    (when (and (< 0 n) (= n (count first-window)))
      (reductions slide first-window the-rest))))

(defn avarage [xs]
  (/ (apply + xs) (count xs)))

(defn nb-moving-avarage [n xs]
  (->> xs (windows n) (map avarage)))
