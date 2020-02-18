(ns miner.moving)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-363-learn-to-build-and-deploy-a-single-page-application/


;; Open question about dropping first couple of averages to get a "central" window and sync
;; up the (time) order.  Or just pre-populate with the start and finish numbers to get a
;; full count of moving average.


;; Probably best to use as much window as possible but clip to 0 and cnt.  Some of my
;; solutions don't do that.  See `moving-avg` for my best effort.


;; BEST SO FAR
;; but assumes fixed size coll (not infinite, lazy)
(defn moving-avg [w coll]
  {:pre [(pos? w)]}
  (let [offset (quot w -2)
        vc (vec coll)
        cnt (count vc)]
    (for [i (range offset (+ cnt offset))
          :let [start (max 0 i)
                end (min cnt (+ i w))]]
      (/ (reduce (fn [sum j] (+ sum (vc j))) 0 (range start end))
         (- end start)))))

;; OK, but not faster.  Seems more complicated.
(defn mavg [w coll]
  {:pre [(pos? w)]}
  (let [vc (vec coll)
        offset (quot w -2)
        cnt (count vc)
        endi (+ cnt offset)]
    (loop [sum (reduce + 0 (subvec vc 0 (+ w (dec offset)))) i offset res []]
      (if (< i endi)
        (let [start (max 0 i)
              end (min cnt (+ i w))
              sum1 (long (- (+ sum (nth vc (dec (+ i w)) 0)) (nth vc (dec i) 0)))]
          ;;(println "i" i  "sum" sum1  "  " start end)
          (recur sum1 (inc i) (conj res (/ sum1 (- end start)))))
        res))))




(defn ben [mav]
  (+ (reduce + (mav 1 (range 10 1000)))
     (reduce + (mav 6 (range 100 10000)))
     (reduce + (mav 7 (range 1000 20000)))))



;; WRONG when n is bigger than (count coll)
;; works, tiny bit faster, but a bit ugly
(defn ravg [n coll]
  (let [vsum (loop [res [(reduce + (take n coll))]
                    subs coll
                    adds (drop n coll)]
               (if (seq adds)
                 (recur (conj res (+ (peek res) (- (first adds) (first subs))))
                        (rest subs)
                        (rest adds))
                 res))
        sn (peek vsum)
        ending (drop (dec (count vsum)) coll)
        n2 (quot n 2)]
    (concat (for [j (range (- n n2) n)]
              (/ (reduce + (take j coll)) j))
            (map #(/ % n) vsum)
            (for [j (range 1 (- n n2))]
              (/ (reduce - sn (take j ending)) (- n j))))))


;; not faster for start
;; (map / (drop n2 (reductions + coll)) (range (inc n2) n))




;; Moving average
(defn simple-moving-average [n coll]
  (->> coll
       (partition n 1)
       (map #(/ (reduce + %) n))))


;; correcting for start-up lag by taking initial half window literally (non-averaged)
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
;; cases with empty or short coll vs wide window, etc.  Notice we're using the somewhat
;; obscure Clojure PersistentQueue as our window collection.  The queue conjes to the end
;; like a vector and pops from the front like a list.  It is Counted so calls to `count` are
;; fast.

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


(defn qavg [w]
  (/ (reduce + w) (count w)))


(defn ma8 [n coll]
  (let [n2 (quot (inc n) 2)
        pre (take n2 coll)]
    (if (< (count pre) n2)
      pre
      (pop (reduce (fn [resw x]
                        (let [w (peek resw)
                              w2 (cond (nil? x) (pop w)
                                       (< (count w) n) (conj w x)
                                       :else (conj (pop w) x))]
                          (conj (pop resw) (qavg w2) w2)))
                      [(qavg pre) (queue pre)]
                      (concat (drop n2 coll) (repeat (dec n2) nil)))))))

;; but not the same -- needs review
(defn ma9 [n coll]
  (let [n2 (quot (inc n) 2)
        pre (take (dec n2) coll)]
    (if (< (count pre) (dec n2))
      pre
      (pop (reduce (fn [resw x]
                        (let [w (peek resw)
                              w2 (cond (nil? x) (pop w)
                                       (< (count w) n) (conj w x)
                                       :else (conj (pop w) x))]
                          (conj (pop resw) (qavg w2) w2)))
                      [(queue pre)]
                      (concat (drop n2 coll) (repeat  n2 nil)))))))





(defn ma81 [n coll]
  (let [window (take n coll)]
    (if (< (count window) n)
      coll
      (let [n2 (quot n 2)
            vwin (reduce (fn [res x] (conj res (conj (pop (peek res)) x)))
                         [(queue window)]
                         (drop n coll))]
        (map qavg (concat (take n2 (drop n2 (reductions conj [] coll)))
                          vwin
                          (take (- (dec n) n2) (rest (iterate pop (peek vwin))))))))))





(defn w82 [n coll]
  (let [window (take n coll)]
    (if (< (count window) n)
      coll
      (let [n2 (quot n 2)
            n1 (- n n2)
            vwin (reduce (fn [res x] (conj res (conj (pop (peek res)) x)))
                         [(queue window)]
                         (drop n coll))]
        (println "n1 =" n1 ", n2 =" n2)
        (map seq (concat (reductions conj (vec (take n1 coll)) (take (dec n2) (drop n1 coll)))
                          vwin
                          (take (dec n1) (rest (iterate pop (peek vwin))))))))))



;; pretty good 
(defn OKw82 [n coll]
  (let [window (take n coll)]
    (if (< (count window) n)
      coll
      (let [n2 (quot n 2)
            n1 (- n n2)
            vwin (reduce (fn [res x] (conj res (conj (pop (peek res)) x)))
                         [(queue window)]
                         (drop n coll))]
        (println "n1 =" n1 ", n2 =" n2)
        (map seq (concat (reductions conj (vec (take n1 window)) (drop n1 window))
                          (rest vwin)
                          (take (dec n1) (rest (iterate pop (peek vwin))))))))))



(defn GOODw82 [n coll]
  (let [window (take n coll)]
    (if (< (count window) n)
      coll
      (let [n2 (quot n 2)
            n1 (- n n2)
            vwin (reduce (fn [res x] (conj res (conj (pop (peek res)) x)))
                         [(queue window)]
                         (drop n coll))]
        (println "n1 =" n1 ", n2 =" n2)
        (map seq (concat (take n2 (reductions conj (vec (take n1 coll)) (drop n1 coll)))
                          vwin
                          (take (dec n1) (rest (iterate pop (peek vwin))))))))))


(defn SAVEw82 [n coll]
  (let [window (take n coll)]
    (if (< (count window) n)
      coll
      (let [n2 (quot n 2)
            n1 (- n n2)
            vwin (reduce (fn [res x] (conj res (conj (pop (peek res)) x)))
                         [(queue window)]
                         (drop n coll))]
        (println "n1 =" n1 ", n2 =" n2)
        (map seq (concat (take n2 (drop n1 (reductions conj [] coll)))
                          vwin
                          (take (dec n1) (rest (iterate pop (peek vwin))))))))))

;; if you don't like to see ratios printed you can add (map ratio) to the above funtion
(defn deratio [rat]
  (if (ratio? rat)
    (double rat)
    rat))

(defn deration-moving-average [n coll]
  (map deratio (moving-average n coll)))

  

(def xyz (mapcat #(repeat 3 %) (range 100)))

;; skipping ends to allow different interpretations
(defn smoke-avg
  ([] (smoke-avg moving-average))
  ([moving]
   (assert (= (moving 1 (range 100)) (range 100)))
   (assert (= (take 90 (drop 5 (moving 5 (range 100)))) (range 5 95)))
   (assert (= (take 16 (drop 2 (moving 4 (interleave (repeat 10 10) (repeat 10 12)))))
              (repeat 16 11)))
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




;; a bit slower to allocate subvecs
(defn mva1 [n coll]
  (let [n2 (quot n 2)
        vc (vec coll)
        cnt (count vc)]
    (for [i (range (- n2) (- cnt n2))
          :let [sv (subvec vc (max 0 i) (min cnt (+ i n)))]]
      (/ (reduce + sv) (count sv)))))


(defn wva [n coll]
  (let [n2 (quot n -2)
        vc (vec coll)
        cnt (count vc)]
    (for [i (range n2 (+ cnt n2))]
      [(max 0 i) (min cnt (+ i n))])))






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
