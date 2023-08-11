(ns miner.binarysearch)

;; My C++ translation did not work so well.  Probably missed something.

;; You would not normally want to do this in Clojure.  Use a set or map if you need fast lookup.

;;; classic, assume V is a vector of sorted ints (or at least comparables)
(defn binsearch [v x]
  (loop [lo 0 hi (count v)]
    (when (< lo hi)
      (let [mid (quot (+ lo hi) 2)
            cmp (compare (v mid) x)]
        (cond (zero? cmp) mid
              (pos? cmp) (recur lo mid)
              :else (recur (inc mid) hi))))))


(def vvv (vec (range 10 100 10)))

(defn test-bs [bs]
  (assert (= (mapv #(bs vvv %) vvv) (range (count vvv))))
  true)

;;; slower.  Tried to translate the C++ code but it didn't really work out.  Improvised, and
;;; it's just a slower version of my natural approach above.
(defn bsearch [v x]
  (let [cnt (count v)]
    (loop [start 0 len cnt]
      (when (pos? len)
        (let [half (quot len 2)
              mid (+ start half)
              cmp (compare (v mid) x)]
          (cond (zero? cmp) mid
                (pos? cmp) (recur start (- len half))
                :else  (recur mid (- len half))))))))
          

;;; https://mhdm.dev/posts/sb_lower_bound/

;; SEM: Note -- I guess first and last are like pointers.

;; template <class ForwardIt, class T, class Compare>
;; constexpr ForwardIt sb_lower_bound(
;;       ForwardIt first, ForwardIt last, const T& value, Compare comp) {
;;    auto length = last - first;
;;    while (length > 0) {
;;       auto rem = length % 2;
;;       length /= 2;
;;       if (comp(first[length], value)) {
;;          first += length + rem;
;;       }
;;    }
;;    return first;
;; }
;; 
;; 
;; 
;; // sb_lower_bound refactored
;; auto length = last - first;
;; while (length > 0) {
;;    auto half = length / 2;
;;    if (comp(first[half], value)) {
;;       // length - half equals half + length % 2
;;       first += length - half;
;;    }
;;    length = half;
;; }
;; return first;
;; 
