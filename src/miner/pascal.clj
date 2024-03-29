(ns miner.pascal)

;;; throw-away example from
;;; "Real @toms with Clojure!" by Thomas Clark and Daniel Slutsky
;;; https://youtu.be/WXtbndYRqBs

;;; the first (pascal) was on a slide as a quick example I was intrigued by the `(... ~@%)
;;; form which I don't remember using.  It makes sense once you think about it as the
;;; backtick is taking a substitution var % within the anonymous fn and ~@ embedding it.
;;; Anyway, I guess the point was to use a bunch of fun punctuation notation.

;;; lots more about Pascal's Triangle
;;; https://en.wikipedia.org/wiki/Pascal%27s_triangle


(defn pascal [] (->> [1] (iterate #(map +' `(0 ~@%) `(~@% 0)))))

;;; SEM: I would change `map` to `mapv` to keep everything as vectors.  But that affects
;;; laziness so if you're doing large things that you might throw away (say) and only take
;;; the last, you have to do a lot of extra work up front.

(defn pascalv [] (->> [1] (iterate #(mapv +' `(0 ~@%) `(~@% 0)))))

;;; or make every item a list
(defn pascal1 [] (->> '(1) (iterate #(map +' `(0 ~@%) `(~@% 0)))))


;;; If I were writing this, I wouldn't be so tricky with the backtick.  Also, there are
;;; better ways to calculate in the Wikipedia article.  But this is just for fun:

(defn pasc []
  (iterate #(mapv +' (conj (seq %) 0) (conj % 0)) [1]))


;;; from wiki, calc nth row directly
(defn pascal-row [n]
  (reductions (fn [p k] (quot (* p (- (inc n) k)) k)) 1 (range 1 (inc n))))
