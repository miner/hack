(ns miner.enewnum)

;;; https://gist.github.com/ericnormand/fb8d0356a5ff7d898707012a97975ec7

;;; New Numbers
;;;
;;; A number is new if its digits are not a permutation of a smaller number. For instance,
;;; 789 is a new number because its permutations (879, 798, 897, 978, and 987) are all
;;; larger than it is. However, 645 is not a new number since 456 and 465 are smaller than
;;; it.  Bonus: You may find a clever way to write new-number?. In addition to that
;;; implementation, implement it in such a way that the definition (no permutations are
;;; smaller) is clear from the code.



(defn new-number? [n]
  (boolean (reduce (fn [a b] (if (pos? (compare a b)) (reduced false) b)) \0 (str n))))


(defn nn1? [n]
  (<= n (Long/parseLong (apply str (sort (str n))))))


(defn nnum? [n]
  (let [digs (seq (str n))]
    (= digs (sort digs))))



(defn smoke-nn [new-number?]
  (assert (new-number? 789))
  (assert (not (new-number? 645)))
  (assert (new-number? 444))
  (assert (new-number? 123456789))
  (assert (not (new-number? 123456780)))
  true)

