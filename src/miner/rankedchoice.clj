(ns miner.rankedchoice)

;;; Winging it.  Don't use this to run a real election!

;;; each vote is a vector in preference order.  First, is top choice, etc.

(def example [[:a :b :c]
              [:b :a :c]
              [:a :b]
              [:c :b :a]
              [:b :c :d]])

(def easy [[:a :c] [:a :b] [:b :c]])

(def easy2 [[:a :b :c] [:a :b :c] [:a :b :c] [:a :b :c]])

(def tie [[:a :b :c :d]
          [:b :c :d :a]
          [:c :b :d :a]
          [:d :b :a :c]])

(def tricky [[:a :b :c :d]
             [:b :c :d :a]
             [:a :b]
             [:b]
             [:c :b :d :a]
             [:d :b :a :c]])




;;; Issue when trying to pick losers out a tie first-place votes.
;;; Should second place votes be a tiebreaker?



(defn high-freq [freqs]
  (reduce (fn [wn [c n :as cn]]
            (cond (= n (peek wn)) (conj (conj (pop wn) c) n)
                  (> n (peek wn)) cn
                  :else wn))
          [0]
          freqs))

;;; needs more testing
;;; fixme on the scanning losers etc. over sorted sfv.  Just take-while
(defn winner1 [votes]
  ;; (println "votes " votes)
  (let [all-candidates (into #{} cat votes)
        majority (inc (quot (count votes) 2))]
    (if (< (count all-candidates) 2)
      (first all-candidates)
      (let [freqs (frequencies (map first votes))
            losers (reduce disj all-candidates (keys freqs))
            sfv (vec (sort-by peek freqs))
            top-sc (peek (peek sfv))
            leaders (filterv #(= (val %) top-sc) sfv)]
        ;; (println " sorted votes" sfv)

        (if (and (>= top-sc majority) (= (count leaders) 1))
          (ffirst leaders)
          (if (seq losers)
            (do  ;; (println " losers" losers)
                (recur (into [] (comp (map #(into [] (remove losers) %)) (remove empty?)) votes)))
            (let [low-sc (peek (first sfv))
                  lowsters (reduce (fn [r [c sc]]
                                        (if (= sc low-sc)
                                          (conj r c)
                                          r))
                                      #{}
                                      sfv)]
              ;; (println "lowsters" low-sc lowsters)
              (if (= low-sc top-sc)
                (list 'tie lowsters)
                (when (seq lowsters)
                  (recur (into [] (comp (map #(into [] (remove lowsters) %))
                                        (remove empty?))
                               votes)))))))))))



(defn winner [votes]
  (let [all-candidates (into #{} cat votes)
        majority (inc (quot (count votes) 2))]
    (if (< (count all-candidates) 2)
      (first all-candidates)
      (let [freqs (frequencies (map first votes))
            losers (reduce disj all-candidates (keys freqs))
            sfv (vec (sort-by val freqs))
            top-sc (val (peek sfv))
            leaders (take-while #(= (val %) top-sc) (rseq sfv))]
        (if (and (>= top-sc majority) (= (count leaders) 1))
          (ffirst leaders)
          (if (seq losers)
            (recur (into [] (comp (map #(into [] (remove losers) %)) (remove empty?)) votes))
            (let [low-sc (peek (first sfv))
                  lowsters (into #{} (comp (take-while #(= low-sc (val %))) (map key)) sfv)]
              (if (= low-sc top-sc)
                (list 'tie lowsters)
                (when (seq lowsters)
                  (recur (into [] (comp (map #(into [] (remove lowsters) %))
                                        (remove empty?))
                               votes)))))))))))


;;; faster way to manage all-candidates
(defn winner2 [votes]
  (loop [votes votes all-candidates (into #{} cat votes)]
    (if (< (count all-candidates) 2)
      (first all-candidates)
      (let [freqs (frequencies (map first votes))
            losers (reduce disj all-candidates (keys freqs))
            sfv (vec (sort-by val freqs))
            top-sc (val (peek sfv))
            leaders (take-while #(= (val %) top-sc) (rseq sfv))]
        (if (and (>= top-sc (inc (quot (count votes) 2))) (= (count leaders) 1))
          (ffirst leaders)
          (if (seq losers)
            (recur (into [] (comp (map #(into [] (remove losers) %)) (remove empty?))
                         votes)
                   (reduce disj all-candidates losers))
            (let [low-sc (peek (first sfv))
                  lowsters (into #{} (comp (take-while #(= low-sc (val %))) (map key)) sfv)]
              (if (= low-sc top-sc)
                (list 'tie lowsters)
                (when (seq lowsters)
                  (recur (into [] (comp (map #(into [] (remove lowsters) %))
                                        (remove empty?))
                               votes)
                         (reduce disj all-candidates lowsters)))))))))))

(defn test-win [winner]
  (assert (= (winner example) :b))
  (assert (= (winner easy) :a))
  (assert (= (winner easy2) :a))
  (assert (= (winner tricky) :b))
  (assert (= (first (winner tie)) 'tie))
  true)

  

;;; fix-me too much scanning, especially winners and losers
;;; should take-while since sfv is sorted!!!

