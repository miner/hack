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

;; faster access
(defn p2 [grid]
  (let [cols (range (count (nth grid 0)))
        at (fn [x y] (nth (nth grid x nil) y 0))]
    (reduce + 0 (for [i (range (count grid))
                      j cols
                      :when (pos? (at i j))]
                  (- 4
                     (at i (inc j))
                     (at i (dec j))
                     (at (dec i) j)
                     (at (inc i) j))))))

(defn p4 [grid]
  (let [cols (range (count (nth grid 0)))
        at (fn [x y] (nth (nth grid x nil) y 0))]
    (* 2 (reduce + 0 (for [i (range (count grid))
                           j cols
                           :when (pos? (at i j))]
                  (- 2
                     (at i (dec j))
                     (at (dec i) j)))))))








;; going for performance
(defn tperim [grid]
  (let [width (count (nth grid 0))
        vflat (persistent! (transduce cat conj! (transient []) grid))]
    (transduce (map-indexed (fn [n x]
                              (if (pos? x)
                                (let [i (quot n width)
                                      row (nth grid i)
                                      j (mod n width)]
                                  (- 4
                                     (nth row (inc j) 0)
                                     (nth row (dec j) 0)
                                     (nth vflat (+ (* width (dec i)) j) 0)
                                     (nth vflat (+ (* width (inc i)) j) 0)))
                                0)))
               +
               vflat)))



(defn tperim2 [grid]
  (let [width (count (nth grid 0))
        vflat (persistent! (transduce cat conj! (transient []) grid))]
    (transduce (map-indexed (fn [n x]
                              (if (zero? x)
                                0
                                (let [i (quot n width)
                                      row (nth grid i)
                                      j (mod n width)]
                                  (- 4
                                     (nth row (inc j) 0)
                                     (nth row (dec j) 0)
                                     (nth vflat (+ (* width (dec i)) j) 0)
                                     (nth vflat (+ (* width (inc i)) j) 0))))))
               +
               vflat)))



;; using harto idea refactored for transducer
;; fast 7.6
(defn tperim6 [grid]
  (let [width (count (nth grid 0))
        v (persistent! (transduce cat conj! (transient []) grid))]
    (* 2 (transduce (map-indexed (fn [n x]
                                   (cond (zero? x) 0
                                         (zero? (rem n width)) (- 2 (nth v (- n width) 0))
                                         :else (- 2 (nth v (dec n)) (nth v (- n width) 0)))))
                    +
                    v))))




;; not fast, but interesting, maybe
(defn zperim [grid]
  (* 2 (reduce + (mapcat (fn [xs prevs aboves]
                           (map (fn [x prev above]
                                  (if (zero? x)
                                    0
                                    (- 2 prev above)))
                                xs prevs aboves))
                         grid
                         (map #(cons 0 %) grid)
                         (cons (repeat 0) grid)))))

;; very slow
(defn zp2 [grid]
  (* 2 (reduce + (sequence (comp (mapcat interleave)
                                 (partition-all 3)
                                 (map (fn [[x prev above]]
                                        (if (zero? x)
                                          0
                                          (- 2 prev above)))))
                           grid
                           (map #(cons 0 %) grid)
                           (cons (repeat 0) grid)))))

;; I want a transducer that takes multiple args out of the stream without having to partition
;; first.  Or think of it as the partition with f applied, but faster.  The simple advantage
;; is that my computation function can use a natural arity instead of structural binding
;; with an extra [[a b c]] or using an apply wrapper.

#_ (do
(quick-bench (reduce + (sequence (mapcat list) (range 10) (range 10 20) (range 20 25))))
(quick-bench (reduce + (interleave (range 10) (range 10 20) (range 20 25))))

(quick-bench (reduce + (take 10 (interleave (range 100) (range 100 200) (range 200 250)))))

)

;; re-implement `interleave` with transducers
;; appears to be slower with slow c's
(defn interleave2
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2] (sequence (mapcat list) c1 c2))
  ([c1 c2 c3] (sequence (mapcat list) c1 c2 c3))
  ([c1 c2 c3 & colls] (apply sequence (mapcat list) c1 c2 c3 colls)))


(defn neighs [rows cols]
  (let [f (fn [x y] (+ y (* cols x)))]
    (for [i (range rows) j (range cols)]
      [i j (f (dec i) j) (f (inc i) j) (f i (dec j)) (f i (inc j))])))


(defn srange
  ([n] (srange 0 n))
  ([beg end]
   (lazy-seq 
    (when (< beg end)
      (Thread/sleep 10)
      (cons beg (srange (inc beg) end))))))

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
   (assert (= (perimeter [[1 1] [0 1]]) 8))
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





(defn harto [grid]
  (letfn [(cell-value [cell x y]
            (if (= 0 cell)
              0
              ;; assume an initial cell perimeter value of 4, then deduct 2 for
              ;; each shared boundary with preceding (i.e. north/west)
              ;; neighbours
              (- 4
                 (* 2 (get-in grid [(dec y) x] 0))
                 (* 2 (get-in grid [y (dec x)] 0)))))]
    (->> grid
         (map-indexed (fn [y row] (map-indexed (fn [x cell] (cell-value cell x y)) row)))
         (apply concat)
         (reduce +))))
