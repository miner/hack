(ns miner.edelim)

;;; https://gist.github.com/ericnormand/3be1ba7dee96abedd8f5809bdf89d2e9
;;;
;;; Longest Delimited Substring
;;;
;;; A delimited string is a string that starts with a character, ends with the same
;;; character, and doesn't contain the character anywhere else (besides the beginning and
;;; end). Here are some examples:
;;;
;;; "abbbbbbba" is delimited because it starts and ends with \a.
;;;
;;; "ajjjjaffa" is not delimited because, though it starts and ends with \a, it also
;;; contains \a inside.
;;;
;;; "bkfifoifu" is not delimited because it doesn't end with the same character it starts
;;; with.
;;;
;;; "aa" is delimited.
;;;
;;; "aufodiufa" is delimited.
;;;
;;; Your task is to write a function that returns the longest
;;; delimited substring of a given string.  In the case of ties, return the substring that
;;; appears first.


(defn char-index-of [s ch from]
  (.indexOf ^String s ^String (str ch) ^int from))

(defn char-last-index-of [s ch from]
  (.lastIndexOf ^String s ^String (str ch) ^int from))


(defn delimited? [s]
  (= (char-index-of s 1 (first s)) (dec (count s))) )

(defn char-groups [s]
  (reduce-kv (fn [m i ch] (update m ch (fnil conj []) i)) {} (vec s)))


;;; need reduce-kv for String, vec works but isn't as fast
;;; note we expect the vals to be in reverse order  (6 3 1)  which simplifies some other
;;; things for diffing and max-key prefering last for ties


(defn delimited0 [s]
  (let [part2 #(partition 2 1 %)
        jidiff (fn [ji] (- (first ji) (second ji)))
        [j i] (->> s
                   vec
                   (reduce-kv (fn [m i ch] (update m ch conj i)) {})
                   vals
                   ;; get rid of < 2 lists
                   (filter next)
                   (mapcat part2)
                   (sort-by #(- (second %)))
                   (apply max-key jidiff [-1 0]))]
    (when (and j (> j i))
      (subs s i (inc j)))))

;; slightly faster to avoid destructuring reduce pairs but longer code
(defn delimited6 [s]
  (let [part2 #(partition 2 1 %)
        jidiff (fn [ji] (- (first ji) (second ji)))
        [j i] (->> s
                   vec
                   (reduce-kv (fn [m i ch] (update m ch conj i)) {})
                   vals
                   ;; get rid of < 2 lists
                   (filter next)
                   (mapcat part2)
                   (reduce (fn [cb ji]
                             (let [c (first cb)
                                   b (second cb)
                                   j (first ji)
                                   i (second ji)
                                   dcb (- c b)
                                   dji (- j i)]
                               (cond (= dcb dji) (if (< i b) ji cb)
                                     (< dcb dji) ji
                                     :else cb)))
                           [-1 0]))]
    (when (and j  (> j i))
      (subs s i (inc j)))))



(defn delimited11 [s]
  (let [[j i] (->> s
                   vec
                   (reduce-kv (fn [m i ch] (assoc! m ch (conj (get m ch) i))) (transient {}))
                   persistent!
                   (filter next)
                   (mapcat #(partition 2 1 (val %)))
                   (reduce (fn [[c b :as cb] [j i :as ji]]
                             (let [dcb (- c b)
                                   dji (- j i)]
                               (cond (= dcb dji) (if (< i b) ji cb)
                                     (< dcb dji) ji
                                     :else cb)))
                           [-1 0]))]
    (when (and j  (> j i))
      (subs s i (inc j)))))

;;; actually (filter next) is faster than letting partition skip
(defn delimited [s]
  (let [[j i] (->> s
                   vec
                   (reduce-kv (fn [m i ch] (update m ch conj i)) {})
                   vals
                   (filter next)
                   (mapcat #(partition 2 1 %))
                   (reduce (fn [[c b :as cb] [j i :as ji]]
                             (let [dcb (- c b)
                                   dji (- j i)]
                               (cond (= dcb dji) (if (< i b) ji cb)
                                     (< dcb dji) ji
                                     :else cb)))
                           [-1 0]))]
    (when (and j  (> j i))
      (subs s i (inc j)))))

(defn xdelimited1 [s]
  (transduce (comp (map val)
                   (filter next)
                   (mapcat #(partition 2 1 %)))
             (fn ([[c b :as cb] [j i :as ji]]
                  (let [dcb (- c b)
                        dji (- j i)]
                    (cond (= dcb dji) (if (< i b) ji cb)
                          (< dcb dji) ji
                          :else cb)))
               ([[j i]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [-1 0]
             (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s))))

(defn xdelimited [s]
  (transduce (comp (filter next)
                   (mapcat #(partition 2 1 %)))
             (fn ([[c b :as cb] [j i :as ji]]
                  (let [dcb (- c b)
                        dji (- j i)]
                    (cond (= dcb dji) (if (< i b) ji cb)
                          (< dcb dji) ji
                          :else cb)))
               ([[j i]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [-1 0]
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))


(defn pair-conj [old i]
  (cond (nil? old) i
        (int? old) [[old i]]
        :else (conj old [(peek (peek old)) i])))

;; fastest so far, surprised the remove int? is faster than filter vector?
(defn xdelimited2 [s]
  (transduce (comp (remove int?) cat)
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [db (- c b)
                        di (- j i)]
                    (cond (= db di) (if (< i b) ij bc)
                          (< db di) ij
                          :else bc)))
               ([[i j]] (when (> j i)
                          (subs s i (inc j)))))
             [0 -1]
             (vals (reduce-kv (fn [m i ch] (update m ch pair-conj i)) {} (vec s)))))

;; not faster
(defn xdelimited25 [s]
  (transduce (comp (filter vector?) cat)
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [db (- c b)
                        di (- j i)]
                    (cond (= db di) (if (< i b) ij bc)
                          (< db di) ij
                          :else bc)))
               ([[i j]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [0 -1]
             (vals (reduce-kv (fn [m i ch] (update m ch pair-conj i)) {} (vec s)))))


(defn xdelimited21 [s]
  (transduce (comp (remove int?) cat)
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [db (- c b)
                        di (- j i)]
                    (cond (> db di) bc
                          (< db di) ij
                          (< i b) ij
                          :else bc)))
               ([[i j]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [0 -1]
             (vals (reduce-kv (fn [m i ch] (update m ch pair-conj i)) {} (vec s)))))

;;; Some improvements  -- faster but I don't like the pair-conj compared to pconj
(defn xdelimited22 [s]
  (transduce (comp (remove int?) cat)
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [c (compare (- c b) (- j i))]
                    (cond (neg? c) ij
                          (pos? c) bc
                          (< i b) ij
                          :else bc)))
               ([[i j]] (when (> j i)
                          (subs s i (inc j)))))
             [0 0]
             (vals (reduce-kv (fn [m i ch] (update m ch pair-conj i)) {} (vec s)))))

(defn xdelimited23 [s]
  (transduce (comp (filter vector?) cat)
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [c (compare (- c b) (- j i))]
                    (cond (neg? c) ij
                          (pos? c) bc
                          (< i b) ij
                          :else bc)))
               ([[i j]] (when (> j i)
                          (subs s i (inc j)))))
             [0 0]
             (vals (reduce-kv (fn [m i ch] (update m ch pair-conj i)) {} (vec s)))))


(defn pconj [old i]
  (if (nil? old)
    [[i i]]
    (conj old [(peek (peek old)) i])))

(defn xdelimited42 [s]
  (transduce cat
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [db (- c b)
                        di (- j i)]
                    (cond (= db di) (if (< i b) ij bc)
                          (< db di) ij
                          :else bc)))
               ([[i j]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [0 0]
             (vals (reduce-kv (fn [m i ch] (update m ch pconj i)) {} (vec s)))))

;; not bad but not the best
(defn xdelimited43 [s]
  (transduce cat
             (fn ([[b c :as bc] [i j :as ij]]
                  (let [c (compare (- c b) (- j i))]
                    (cond (zero? c) (if (< i b) ij bc)
                          (neg? c) ij
                          :else bc)))
               ([[i j]] (when (> j i)
                          (subs s i (inc j)))))
             [0 0]
             (vals (reduce-kv (fn [m i ch] (update m ch pconj i)) {} (vec s)))))

(defn xdelimited4 [s]
  (transduce cat
             (fn ([[b c :as bc] [i j :as ij]]
                  (if (neg? i)
                    bc
                    (let [db (- c b)
                          di (- j i)]
                      (cond (= db di) (if (< i b) ij bc)
                            (< db di) ij
                            :else bc))))
               ([[i j]] (when (and j (> j i))
                          (subs s i (inc j)))))
             [0 -1]
             (vals (reduce-kv (fn [m i ch] (update m ch pconj i)) {} (vec s)))))


;;; NEW IDEA:  pairs should be [len start] instead of end -- but we need the previous end so
;;; maybe triplets

;;; Don't forget to try str-reduce-kv


;; don't have to filter, part2 will effectively drop the singles, but it's actually slower
;; so probably keep the filter

;; not faster
(defn gdelimited [s]
  (let [part2 #(partition 2 1 (rseq %))
        [j i] (->> (range (count s))
                   (group-by #(.charAt ^String s ^int %))
                   vals
                   (mapcat part2)
                   (reduce (fn [[c b :as cb] [j i :as ji]]
                             (let [dcb (- c b)
                                   dji (- j i)]
                               (cond (= dcb dji) (if (< i b) ji cb)
                                     (< dcb dji) ji
                                     :else cb)))
                           [-1 0]))]
    (when (and j  (> j i))
      (subs s i (inc j)))))






(defn max-step [is]
  (let [[i j] (reduce (fn [[i j] k] (if (> (- k j) (- j i)) [j k] [i j]))
                       [(inc (first is)) (first is)]
                       (rest is))]
    (when (> j i)
      [i j])))

(defn jdiff [[i j]]
  (if (nil? i)
    -1
    (- j i)))
  

(defn delimited1 [s]
  (let [cg (char-groups s)
        [i j] (apply max-key jdiff [0 -1] (sort-by #(- (first %)) (remove nil? (map max-step (vals cg)))))]
    (when (> j i)
      (subs s i (inc j)))))
    


;; good idea but much slower
;; convert rs (6 3 1) into [len start] -- but see below for a modified len (really (dec len))
(defn xdelimited7 [s]
  (transduce (mapcat (fn [rs] (map list (map (comp inc -) rs (rest rs)) (rest rs))))
             (fn ([[d i :as di] [e j :as ej]]
                  (cond (= d e) (if (< i e) di ej)
                        (< d e) ej
                        :else di))
               ([di] (when (pos? (first di))
                       (subs s (second di) (+ (first di) (second di))))))
             '(0 0)
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))

(defn xdelimited71 [s]
  (transduce (mapcat (fn [rs] (map list (map - (map inc rs) (rest rs)) (rest rs))))
             (fn ([[d i :as di] [e j :as ej]]
                  (cond (= d e) (if (< i e) di ej)
                        (< d e) ej
                        :else di))
               ([di] (when (pos? (first di))
                       (subs s (second di) (+ (first di) (second di))))))
             '(0 0)
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))

(defn xdelimited72 [s]
  (transduce (comp (map (fn [rs]
                          (cond (nnext rs) (apply max-key first
                                                  (map list (map - rs (rest rs))
                                                       (rest rs)))
                                (next rs) (list (- (first rs) (second rs))
                                                (second rs))
                                :else nil)))
                   (remove nil?))
                        
             (fn ([[d i :as di] [e j :as ej]]
                  (cond (= d e) (if (< i e) di ej)
                        (< d e) ej
                        :else di))
               ([di] (when (pos? (first di))
                       (subs s (second di) (+ (first di) (inc (second di)))))))
             '(0 0)
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))


(defn best-rs [rs]
  (cond (nnext rs)  (rest (reduce (fn [[pr len st :as longest] r]
                              (let [lenr (- pr r)]
                                ;;(println (list len st pr) (list lenr r))
                                (if (>= lenr len)
                                  (list r lenr r)
                                  (cons r (rest longest)))))
                            (list (second rs) (- (first rs) (second rs))  (second rs))
                            (nnext rs)))
        (next rs) (list (- (first rs) (second rs)) (second rs))
        :else nil))


(defn simple-rs [rs]
  (->> rs
       (partition 2 1)
       (map (fn [[end st]] (list (- end st) st)))
       (apply max-key first '(-1 -1))))
      
;; still not so fast
(defn xdelimited73 [s]
  (transduce (comp (map best-rs)
                   (remove nil?))
                        
             (fn ([[d i :as di] [e j :as ej]]
                  (cond (= d e) (if (< i e) di ej)
                        (< d e) ej
                        :else di))
               ([di] (when (pos? (first di))
                       (subs s (second di) (+ (first di) (inc (second di)))))))
             '(-1 -1)
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))

;; very slow
(defn xdelimited74 [s]
  (transduce (map simple-rs)
             (fn ([[d i :as di] [e j :as ej]]
                  (cond (= d e) (if (< i e) di ej)
                        (< d e) ej
                        :else di))
               ([di] (when (pos? (first di))
                       (subs s (second di) (+ (first di) (inc (second di)))))))
             '(-1 -1)
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))


(defn smoke-delim [delimited]
  (assert (= (delimited "ffdsfuiofl") "fuiof"))
  (assert (= (delimited "abbcdefg") "bb"))
  (assert (= (delimited "opoiifdopf") "poiifdop"))
  (assert (= (delimited "abcaba") "abca"))
  (assert (nil? (delimited "abc")))
  (assert (nil? (delimited "a")))
  (assert (nil? (delimited "")))
  true)


