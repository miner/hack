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


(defn mvgavg [n coll]
  ;; sequence for lazy
  (into [] (map #(/ (reduce + %) n)) (partition n 1 coll)))




;; if you don't like to see ratios printed you can add (map ratio) to the above funtion
(defn deratio [rat]
  (if (ratio? rat)
    (double rat)
    rat))

(defn deration-moving-average [n coll]
  (map deratio (moving-average n coll)))

  

(def xyz (mapcat #(repeat 3 %) (range 100)))

(defn smoke-avg []
  (assert (= (moving-average 1 (range 100)) (range 100)))
  (assert (= (moving-average 5 (range 100)) (range 100)))
  (assert (= (moving-average 4 (interleave (repeat 10 10) (repeat 10 12)))
             '(10 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 10 12)))
  true)





;; not so pretty
(defn moving-average1 [n coll]
  (let [pre (quot (dec n) 2)
        cnt (count coll)]
    (concat (take pre coll)
            (->> coll
                 (partition n 1)
                 (map #(/ (reduce + %) n)))
            (drop (- cnt pre (if (odd? (+ cnt pre)) 1 0)) coll))))
