(ns advent21.adv11
  (:require [clojure.string :as str]))


(def sample-input
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn parse-grid [input]
  (mapv (fn [line] (mapv #(Long/parseLong (str %)) line)) (str/split-lines input)))


(defn neighbor-xys [[x y]]
  [[(dec x) (dec y)]
   [(dec x) y]
   [(dec x) (inc y)]
   [x (dec y)]
   [x (inc y)]
   [(inc x) (dec y)]
   [(inc x) y]
   [(inc x) (inc y)]])

(defn at [grid x y]
  (nth (nth grid x nil) y nil))

(defn inc-grid [grid]
  (mapv #(mapv inc %) grid))

(defn find-tens [grid]
  (for [x (range (count grid))
        y (range (count (peek grid)))
        :when (>= (at grid x y) 10)]
    [x y]))

(defn inc-at [grid [x y :as pt]]
  (if-let [p (at grid x y)]
    (assoc-in grid pt (inc p))
    grid))

(defn flash-tens [grid pts]
  (reduce (fn [g xy]
            (reduce inc-at (assoc-in g xy -99) (neighbor-xys xy)))
          grid
          pts))

(defn clear-flashes [grid]
  (mapv (fn [row] (mapv #(if (neg? %) 0 %) row)) grid))


(defn step-grid [state]
  #_ (do (println)
      (println "step")
      (clojure.pprint/pprint state))

  (loop [flashes (:flashes state)  g2 (inc-grid (:grid state))]
    (let [tens (find-tens g2)
          cnt (count tens)]
      #_(do (println " loop ")
         (clojure.pprint/pprint g2))
      (if (zero? cnt)
        {:grid (clear-flashes g2) :flashes flashes}
        (recur (+ flashes cnt) (flash-tens g2 tens))))))

(defn run-grid [cnt input]
  (loop [n cnt state {:flashes 0 :grid (parse-grid input)}]
    (if (zero? n)
      (:flashes state)
      (recur (dec n) (step-grid state)))))

(defn all-zero? [grid]
  (every? zero? (sequence cat grid)))

(defn steps-all-zero? [input]
  (loop [n 0 state {:flashes 0 :grid (parse-grid input)}]
    (if (all-zero? (:grid state))
      n
      (recur (inc n) (step-grid state)))))
        
(def real-input
"4112256372
3143253712
4516848631
3783477137
3746723582
5861358884
4843351774
2316447621
6643817745
6366815868")
