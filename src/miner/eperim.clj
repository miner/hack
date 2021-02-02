(ns miner.eperim)

;; https://gist.github.com/ericnormand/93901ecca1dadf188de40626847ba933

;; Land perimeter
;; 
;; A grid of 1s and 0s shows the location of land and water. A 1 represents a square full of
;; land, a 0 represents a square full of water. Your job is to calculate the perimeter of
;; the land, both as it touches water and touches the edge.



(defn zneighbors [grid i j]
  (if (zero? (get-in grid [i j]))
    0
    (cond-> 0
      (zero? (get-in grid [(dec i) j] 0)) inc
      (zero? (get-in grid [i (inc j)] 0)) inc
      (zero? (get-in grid [i (dec j)] 0)) inc
      (zero? (get-in grid [(inc i) j] 0)) inc)))

(defn perimeter [grid]
  (reduce + 0 (for [i (range (count grid))
                    j (range (count (nth grid i)))]
                (zneighbors grid i j))))



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
