(ns miner.eperim)

;; https://gist.github.com/ericnormand/93901ecca1dadf188de40626847ba933

;; Land perimeter
;; 
;; A grid of 1s and 0s shows the location of land and water. A 1 represents a square full of
;; land, a 0 represents a square full of water. Your job is to calculate the perimeter of
;; the land, both as it touches water and touches the edge.


(defn perimeter [grid]
  (reduce + 0 (for [i (range (count grid))
                    :let [row (nth grid i)]
                    j (range (count row))
                    :when (pos? (nth row j))]
                (- 4
                   (nth row (inc j) 0)
                   (nth row (dec j) 0)
                   (get-in grid [(dec i) j] 0)
                   (get-in grid [(inc i) j] 0)))))

;; not much diff
(defn p6 [grid]
  (reduce + 0 (for [i (range (count grid))
                    :let [row (nth grid i)]
                    j (range (count row))
                    :when (pos? (nth row j))]
                (- 4
                   (+ (+ (nth row (inc j) 0)
                         (nth row (dec j) 0))
                      (+ (get-in grid [(dec i) j] 0)
                         (get-in grid [(inc i) j] 0)))))))

(defn tperim [grid]
  (let [width (count (nth grid 0))]
    (transduce (comp cat
                     (map-indexed (fn [n x]
                                    (if (pos? x)
                                    (let [i (quot n width)
                                          row (nth grid i)
                                          j (mod n width)]
                                        (- 4
                                           (nth row (inc j) 0)
                                           (nth row (dec j) 0)
                                           (get-in grid [(dec i) j] 0)
                                           (get-in grid [(inc i) j] 0)))
                                        0))))
               +
               grid)))




(defn smoke-perim
  ([] (smoke-perim perimeter))
  ([perimeter]
   ;; A grid with a single square filled with land has a perimeter of 4, since there are four sides:
   (assert (= (perimeter [[1]])  4))
   ;;Likewise, a single square filled with water has a perimeter of 0:
   (assert (= (perimeter [[0]])  0))
   ;;Two squares of land next to each other share an edge, which reduces the perimeter:
   (assert (= (perimeter [[1 1]])  6))
   ;; The edge of the grid is like an implicit encircling of water:
   (assert (= (perimeter [[1 1]
                          [1 1]])  8))
   (assert (= (perimeter [[0 0 0 0]
                          [0 1 1 0]
                          [0 1 1 0]
                          [0 0 0 0]]) 8))
   ;;Here are some other weird shapes:
   (assert (= (perimeter [[1 1 1 1 1 1]
                          [1 0 0 0 0 1]
                          [1 0 1 1 0 1]
                          [1 0 0 0 0 1]
                          [1 1 1 1 1 1]]) 42))
   (assert (= (perimeter [[0 1 0 0]
                          [1 1 1 0]
                          [0 1 0 0]
                          [1 1 0 0]])  16))
   true))


(def sample [[0 1 0 0]
             [1 1 1 0]
             [0 1 0 0]
             [1 1 0 0]])

(def all [[1 1] [1 1]])



(defn sw-perimeter [rows]
  (->> rows (apply map vector) (concat rows) (transduce (mapcat dedupe) +) (* 2)))

;; note creating small vectors is faster that lists (at least in this case)
;; pretty good improvement with sequence vs (sw2)
(defn sw22 [rows]
  (* 2 (+ (transduce (mapcat dedupe) + rows)
          (transduce (mapcat dedupe) + (apply sequence (map vector) rows)))))

(defn sw3 [rows]
  (* 2 (transduce (mapcat dedupe) + (transduce (mapcat dedupe) + rows)
                  (apply sequence (map vector) rows))))

;; not necessarily worth it to shift cat
(defn sw4 [rows]
  (* 2 (transduce (dedupe) + (transduce (mapcat dedupe) + rows)
                  (apply sequence (mapcat list) (repeat 0) rows ))))



(defn zneighbors [grid i j]
  (if (zero? (get-in grid [i j]))
    0
    (cond-> 0
      (zero? (get-in grid [(dec i) j] 0)) inc
      (zero? (get-in grid [i (inc j)] 0)) inc
      (zero? (get-in grid [i (dec j)] 0)) inc
      (zero? (get-in grid [(inc i) j] 0)) inc)))

(defn perimeter1 [grid]
  (reduce + 0 (for [i (range (count grid))
                    j (range (count (nth grid i)))]
                (zneighbors grid i j))))


(defn perimeter2 [grid]
  (reduce + (for [i (range (count grid))
                  j (range (count (nth grid i)))
                  :when (not (zero? (get-in grid [i j])))]
              (cond-> 0
                (zero? (get-in grid [(dec i) j] 0)) inc
                (zero? (get-in grid [i (inc j)] 0)) inc
                (zero? (get-in grid [i (dec j)] 0)) inc
                (zero? (get-in grid [(inc i) j] 0)) inc))))

(defn perimeter3 [grid]
  (reduce + 0 (for [i (range (count grid))
                    j (range (count (nth grid i)))
                    :when (pos? (get-in grid [i j]))]
                (- 4
                   (get-in grid [(dec i) j] 0)
                   (get-in grid [i (inc j)] 0)
                   (get-in grid [i (dec j)] 0)
                   (get-in grid [(inc i) j] 0)))))




;; slower
(defn perimeter4 [grid]
  (reduce + 0 (for [i (range (count grid))
                    j (range (count (nth grid i)))
                    :when (pos? (get-in grid [i j]))]
                (reduce - 4 (map #(get-in grid % 0) [[(dec i) j]
                                                     [i (inc j)]
                                                     [i (dec j)]
                                                     [(inc i) j]])))))
;; slower
(defn perimeter5 [grid]
  (reduce + 0 (for [i (range (count grid))
                    j (range (count (nth grid i)))
                    :when (pos? (get-in grid [i j]))
                    [h v] [[-1 0] [0 1] [0 -1] [1 0]]
                    :when (zero? (get-in grid [(+ i v) (+ j h)] 0))]
                1)))

