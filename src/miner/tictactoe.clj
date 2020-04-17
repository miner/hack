(ns miner.tictactoe)

;; 373 - PurelyFunctional.tv Newsletter
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-373-don-t-sweat-the-stack/
;;
;; https://gist.github.com/ericnormand/9344789ccf1cceebb87d8370aa3d69c5


(def bbb [[0 1 2]
          [3 4 5]
          [6 7 8]])

(def xxx [[:x :o :x]
          [:o :x :o]
          [:o :x :x]])

(def ooo [[:o :o :o]
          [:o :x :x]
          [nil :x :x]])

(def ddd [[:x :x :o]
          [:o :o :x]
          [:x :x :o]])


;; only for exactly 3x3, but fast
(defn winner-jp3 [[[a b c]
                   [d e f]
                   [g h i]]]
  (-> (filter #(apply = %)
              [[a b c] [d e f] [g h i]   ;; horizontals
               [a e i] [c e g]           ;; diagonals
               [a d g] [b e h] [c f i]]) ;; verticals
      ffirst
      (or :draw)))



;; best, fastest especially with large boards (10x10)
;; inspired by g7s solution

(defn winner [board]
  (let [winr #(when (apply = %) (first %))
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))



;; can be faster with faster winr but uglier

(defn wr [row]
  (loop [res (first row) row (next row)]
    (cond (nil? row) res
          (nil? res) nil
          (= res (first row)) (recur res (next row))
          :else nil)))
    
(defn winner99 [board]
  (let [winr wr
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))



;; FASTEST but not my top pick.
;; slightly faster with ugly winr
(defn winner93 [board]
  (let [winr (fn [r] (when-let [x (first r)] (when (every? #(= x %) (rest r)) x)))
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))

(defn winner95 [board]
  (let [winr (fn [r] (reduce #(if (= % %2) %1 (reduced nil)) (first r) (rest r)))
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))


;; not so fast
(defn rwinr [row]
  (reduce (fn [res x] (if (= res x) res (reduced nil))) (first row) (rest row)))


;;; BUT benchmark is sensitive to row/diag test order.

;; fastest, slightly
(defn winner5 [board]
  (let [winr (fn [row] (let [x (first row)] (when (every? #(= x %) (rest row)) x)))]
    (or (first (keep winr board))
        (let [dim (count board)
              rng (range dim)]
          (or (winr (for [i rng] (get-in board [i i])))
              (winr (for [i rng] (get-in board [(- (dec dim) i) i])))
              (first (keep (fn [col] (winr (map #(get-in board [% col]) rng))) rng))))
        :draw)))



(defn winner53 [board]
  (let [winr (fn [row] (let [x (first row)] (when (every? #(= x %) (rest row)) x)))]
    (or (first (keep winr board))
        (let [dim (count board)
              rng (range dim)]
          (or (winr (for [i rng] (get-in board [i i])))
              (winr (for [i rng] (get-in board [(- (dec dim) i) i])))
              (first (keep winr (apply map list board)))
              :draw)))))




;; prettier
(defn winner6 [board]
  (let [winr (fn [row] (when (apply = row) (first row)))]
    (or (first (keep winr board))
        (let [dim (count board)
              rng (range dim)]
          (or (first (keep (fn [col] (winr (map #(get-in board [% col]) rng))) rng))
              (winr (for [i rng] (get-in board [i i])))
              (winr (for [i rng] (get-in board [(- (dec dim) i) i])))))
        :draw)))


;; from gist comment by g7s
;; I think it's clever.  But slightly slower than my winner5
(defn winner-g7s
  [rows]
  (let [cols   (apply map vector rows)
        diag-1 (map get rows (range))
        diag-2 (map get rows (range (dec (count rows)) -1 -1))]
    (or (->> (concat rows cols [diag-1 diag-2])
             (filter (partial apply =))
             ffirst)
        :draw)))


;; my mods -- now fastest but actually a bit slower on my benchmark with 10x10
(defn winner7 [board]
  (let [winr (fn [r] (let [x (first r)] (when (every? #(= x %) (rest r)) x)))
        dim (count board)]
    (or (winr (map get board (range dim)))
        (winr (map get board (range (dec dim) -1 -1)))
        (first (keep winr board))
        (first (keep winr (apply map list board)))
        :draw)))


(defn winner8t [board]
  (let [winr (fn [r] (let [x (first r)] (when (every? #(= x %) (rest r)) x)))
        dim (count board)]
    (or (winr (map get board (range dim)))
        (winr (map get board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (sequence (keep winr) (apply map list board)))
        :draw)))


;; much faster with transducers
(defn winner9t [board]
  (let [winr (fn [r] (let [x (first r)] (when (every? #(= x %) (rest r)) x)))
        dim (count board)]
    (or (winr (map get board (range dim)))
        (winr (map get board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (sequence (keep winr) (apply sequence (map list) board)))
        :draw)))







(defn winner72 [board]
  (let [winr (fn [row] (let [x (first row)] (when (every? #(= x %) (rest row)) x)))]
    (or (first (keep winr board))
        (let [dim (count board)]
          (or (first (keep winr (apply map list board)))
              (winr (map get board (range dim)))
              (winr (map get board (range (dec dim) -1 -1)))))
        :draw)))

(defn winner71 [board]
  (let [winr (fn [row] (when (apply = row) (first row)))]
    (or (first (keep winr board))
        (let [dim (count board)]
          (or (first (keep winr (apply map list board)))
              (winr (map get board (range dim)))
              (winr (map get board (range (dec dim) -1 -1)))))
        :draw)))



(defn winner0 [board]
  (let [dim (count board)
        cols (for [col (range dim)]
               (map #(get-in board [% col]) (range dim)))
        diag1 (for [i (range dim)] (get-in board [i i]))
        diag2 (for [i (range dim)] (get-in board [(- (dec dim) i) i]))]
    (or (ffirst (filter #(apply = %)
                        (concat board cols (list diag1 diag2))))
        :draw)))





(defn winner1 [board]
  (let [dim (count board)]
    (or (ffirst (filter #(apply = %) board))
        (ffirst (filter #(apply = %)
                        (for [col (range dim)]
                          (map #(get-in board [% col]) (range dim)))))
        (when (apply = (for [i (range dim)] (get-in board [i i])))
          (get-in board [0 0]))
        (when (apply = (for [i (range dim)] (get-in board [(- (dec dim) i) i])))
          (get-in board [(dec dim) 0]))
        :draw)))



(defn winner2 [board]
  (let [dim (count board)]
    (or (ffirst (filter #(apply = %) board))
        (ffirst (filter (fn [col] (apply = (map #(get-in board [% col]) (range dim))))
                        (range dim)))
        (when (apply = (for [i (range dim)] (get-in board [i i])))
          (get-in board [0 0]))
        (when (apply = (for [i (range dim)] (get-in board [(- (dec dim) i) i])))
          (get-in board [(dec dim) 0]))
        :draw)))



(defn winr [row]
  (let [x (first row)]
    (when (every? #(= x %) (rest row))
      x)))


(defn winner3 [board]
  (let [dim (count board)]
    (or (first (keep winr board))
        (first (keep (fn [col] (winr (map #(get-in board [% col]) (range dim))))
                       (range dim)))
        (winr (for [i (range dim)] (get-in board [i i])))
        (winr (for [i (range dim)] (get-in board [(- (dec dim) i) i])))
        :draw)))


(defn winner4 [board]
    (or (first (keep winr board))
        (let [dim (count board)
              rng (range dim)]
          (or (first (keep (fn [col] (winr (map #(get-in board [% col]) rng))) rng))
              (winr (for [i rng] (get-in board [i i])))
              (winr (for [i rng] (get-in board [(- (dec dim) i) i])))))
        :draw))







(defn winrow [row]
  (when (apply = row) (peek row)))

(defn winv [v is]
  (when-first [i is]
    (when (apply = (map v is))
      (v i))))

(defn winnerOK [board]
  (or (first (map winrow board))
      (let [v (into [] cat board)
            vwin #(winv v %)
            cnt (count v)
            dim (count board)]
        (or (first (keep vwin (for [col (range dim)] (range col cnt dim))))
            (vwin (range 0 cnt (inc dim)))
            (vwin (range (dec dim) (dec cnt) (dec dim)))))
      :draw))


(defn smoke-tsmall [winner]
  (assert (= (winner [[:x :o :x]
                      [:o :x :o]
                      [:o :x :x]])
             :x))
  
  (assert (= (winner [[:o :o :o]
                      [:o :x :x]
                      [nil :x :x]])
             :o))
  
  (assert (= (winner [[:x :x :o]
                      [:o :o :x]
                      [:x :x :o]])
             :draw))
  true)

  
(defn smoke-ttt [winner]
  (assert (= (winner [[:x :o :x]
                      [:o :x :o]
                      [:o :x :x]])
             :x))
  
  (assert (= (winner [[:o :o :o]
                      [:o :x :x]
                      [nil :x :x]])
             :o))
  
  (assert (= (winner [[:x :x :o]
                      [:o :o :x]
                      [:x :x :o]])
             :draw))
  (assert (= (winner [[:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:o :x :x :x :x :x :x :x :x :o]
                      [:x :o :o :o :o :o :o :o :o :x]])
             :x))
  (assert (= (winner [[:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:o :x :x :x :x :x :x :x :x :o]
                      [:x :o :o :o :o :o :o :o :x :o]])
             :x))
  (assert (= (winner [[:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:o nil nil nil nil nil nil nil nil :o :o]])
             :draw))
  true)


(def x10 [[:x :x :x :x :x :x :x :x :x nil]
          [:x :x :x :x :x :x :x :x :x nil]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:o :x :x :x :x :x :x :x :x :o]
          [:x :o :o :o :o :o :o :o :o :x]])

(def d10 [[:x :x :x :x :x :x :x :x :x nil]
          [:x :x :x :x :x :x :x :x :x nil]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:x :x :x :x :x :x :x :x :x :o]
          [:o nil nil nil nil nil nil nil nil :o :o]])
