(ns miner.tictactoe)

;; 373 - PurelyFunctional.tv Newsletter
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-373-don-t-sweat-the-stack/
;;
;; https://gist.github.com/ericnormand/9344789ccf1cceebb87d8370aa3d69c5


(def bbb [[0 1 2]
          [3 4 5]
          [6 7 8]])

;; clever bit by g7s
;; for our purposes, I translated into a faster transducer version, but same idea
(defn transpose [matrix-2d]
  (apply mapv vector matrix-2d))

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



;; my favorite, fastest with large boards (10x10)
;; every? expr slightly faster than apply =
;; inspired by g7s solution
(defn winner [board]
  (let [winr (fn [[x & xs]] (when (every? #(= x %) xs) x))
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))


;; pretty good
(defn winner-good [board]
  (let [winr #(when (apply = %) (first %))
        dim (count board)]
    (or (winr (map nth board (range dim)))
        (winr (map nth board (range (dec dim) -1 -1)))
        (first (sequence (keep winr) board))
        (first (apply sequence (comp (map list) (keep winr)) board))
        :draw)))


;; can be faster with faster winr but uglier

;; FASTEST but not my top pick.
;; slightly faster with ugly winr




;;; BUT benchmark is sensitive to row/diag test order.

(defn wr [row]
  (loop [res (first row) row (next row)]
    (cond (nil? row) res
          (nil? res) nil
          (= res (first row)) (recur res (next row))
          :else nil)))

;; reduce version wasn't any faster


;; `for` not as elegant
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
;; I think it's clever.  But somewhat slower than it could be.  See my winner for an improvement
(defn winner-g7s
  [rows]
  (let [cols   (apply map vector rows)
        diag-1 (map get rows (range))
        diag-2 (map get rows (range (dec (count rows)) -1 -1))]
    (or (->> (concat rows cols [diag-1 diag-2])
             (filter (partial apply =))
             ffirst)
        :draw)))





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
  (assert (= (winner [[:x]]) :x))
  (assert (= (winner [[:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x nil]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :o]
                      [:x :x :x :x :x :x :x :x :x :x]
                      [:x :o :o :o :o :o :o :o :o :o]])
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
