;; https://gist.github.com/ericnormand/087eab23272b3ed0d7a8e3007b699a1d
;;
;; Eric Challenge: The Frugal Gentleman
;; find second cheapest wine
;; tie case is not well-defined

(ns miner.ewine)

(def sample-wines [{:name "White" :price 10.99}
                   {:name "Red"   :price 8.74}
                   {:name "Rosé"  :price 12.22}])


;; clear and slightly faster
(defn choose-wine [wines]
  (if (next wines)
    (:name (second (sort-by :price wines)))
    (:name (first wines))))


;; fastest (slightly) so far,
;; but of course longer and harder to read than the simple sort
;; need special case for fewer than two wines
(defn red-wine [wines]
  (if-let [rws (seq (rest wines))]
    (-> (reduce (fn [[a b :as ab] w]
                  (let [wp (:price w)]
                    (cond (>= wp (:price b)) ab
                          (< wp (:price a)) [w a]
                          :else [a w])))
                [(first wines)
                 {:price Double/MAX_VALUE}]
                rws)
        peek
        :name)
    (:name (first wines))))





(defn wlt [wa wb]
  (< (:price wa) (:price wb)))

(defn rwine [wines]
  (if-let [rws (seq (rest wines))]
    (-> (reduce (fn [[a b :as ab] w]
                    (cond (wlt b w) ab
                          (wlt w a) [w a]
                          :else [a w]))
                [(first wines)
                 {:price Double/MAX_VALUE}]
                rws)
        peek
        :name)
    (:name (first wines))))






;; not faster
(defn red-wine2 [wines]
  (if-let [rws (next wines)]
    (-> (reduce (fn [[a b :as ab] w]
                  (let [wp (:price w)]
                    (cond (>= wp (:price b Double/MAX_VALUE)) ab
                          (< wp (:price a)) [w a]
                          :else [a w])))
                [(first wines) nil]
                rws)
        peek
        :name)
    (:name (first wines))))


;; not faster
(defn lowine [wines]
  (loop [ws (rest wines) a (first wines) b nil]
    (if (seq ws)
      (let [w (first ws)
            wp (:price w)]
        (cond (>= wp (:price b Double/MAX_VALUE)) (recur (rest ws) a b)
              (< wp (:price a)) (recur (rest ws) w a)
              :else (recur (rest ws) a w)))
      (:name (or b a)))))







(defn smoke-wine [choose-wine]
  (assert (= (choose-wine [{:name "White" :price 10.99}
                           {:name "Red"   :price 8.74}
                           {:name "Rosé"  :price 12.22}])
             "White"))
  (assert (= (choose-wine [{:name "White" :price 10.99}])
             "White"))
  (assert (= (choose-wine [{:name "White" :price 10.99}
                           {:name "Rosé"  :price 12.22}])
             "Rosé"))
  (assert (= (choose-wine [{:name "Rosé"  :price 12.22}
                           {:name "White" :price 10.99}])
             "Rosé"))
  (assert (nil? (choose-wine [])))
  (assert (= (choose-wine (list* {:name "White" :price 10.99}
                                 {:name "Red"   :price 8.74}
                                 (repeat 1000 {:name "Rosé"  :price 12.22})))
             "White"))
  (assert (= (choose-wine (concat (repeat 1000 {:name "Rosé"  :price 12.22})
                                  (list {:name "White" :price 10.99}
                                        {:name "Red"   :price 8.74})))
             "White"))
  true)

