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

;;; Issue: didn't say anything about case sensitivity.  I assume case-sensitive is
;;; appropriate and it's simpler.

;;; reduce-kv needs (vec s) -- bug pending to add kv-reduce on String directly.
;;; Effectively, we get all the indices for each letter (like a group-by) with a descending
;;; order of indices in each val, by consruction.  Knowing the order, lets us convert to
;;; terms of [distance -start] by taking successive (descending) indices (end st).  Like
;;; taking (partition 2 1 ...) but specialized and faster logic.  We make the st neg so that
;;; (compare ...) will gives us the desired result on little vector values.  We want the
;;; maximum distance but the minimum start on ties.  (filter next) is a quicker way to skip
;;; single chars.  There are ways to make this slightly faster (see my xdelimited2) but the
;;; code is even more obscure.  This seems like a reasonable compromise.
;;;
;;; Tricky representation: di = [distance -start]
(defn delimited [s]
  (transduce (comp (filter next)
                   (mapcat (fn [rs] (map (fn [end st] (vector (- end st) (- st))) rs (rest rs)))))
             (fn ([di ej] (if (neg? (compare di ej)) ej di))
               ([[d i]] (when (pos? d) (subs s (- i) (- (inc d) i)))))
             [-1]
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))

;;; but see r2-delimited for the fastest bestest approach using the regex from sw


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



;;; actually (filter next) is faster than letting partition skip
(defn delimited1 [s]
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

;; (vals m)  has internal implementation so faster than xform (map val) later


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


;;; Some improvements  -- faster but I don't like the pair-conj compared to pconj.  On the
;;; other hand, pair-conj is a bit faster so don't throw it away.  (remove int?) is
;;; surprisingly fast, too.

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
  

(defn delimited3 [s]
  (let [cg (char-groups s)
        [i j] (apply max-key jdiff [0 -1] (sort-by #(- (first %)) (remove nil? (map max-step (vals cg)))))]
    (when (> j i)
      (subs s i (inc j)))))
    


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



;;; new idea, push best len through and avoid creating little collections


;; Tricky: neg start which makes compare work appropriately for [diff start] semantics.
;; Have to flip the sign to get the actual substring.

;; Note: we know that no two vectors can be exactly equal by construction (as they start from
;; unique places).  So that simplifies using compare.  We want to take the later one.  We
;; arranged the starts to be in reverse order, by conj-ing onto a list.

;; winning, not quite as fast as xd2 but pretty good, and short
;; proposed solution
(defn xdelimited87 [s]
  (transduce (comp (filter next)
                   (mapcat (fn [rs] (map (fn [end st] (vector (- end st) (- st))) rs (rest rs)))))
             (fn ([di ej] (if (neg? (compare di ej)) ej di))
               ([[d i]] (when (pos? d) (subs s (- i) (- (inc d) i)))))
             [-1]
             (vals (reduce-kv (fn [m i ch] (update m ch conj i)) {} (vec s)))))


;;; Too confusing to submit, but you can save a (vec s) if you add this
#_
(if (satisfies? clojure.core.protocols/IKVReduce "foobar")
  (throw (ex-info
          "Surprise: someone added IKVReduce support for String.  Skipping my implementation."
          {::surprise :IKVReduce }))
  (extend-protocol clojure.core.protocols/IKVReduce
    java.lang.String
    (kv-reduce [^String s f init]
      (let [cnt (.length s)]
        (loop [i 0 res init]
          (if (< i cnt)
            (let [ret (f res i (.charAt s i))]
              (if (reduced? ret)
                @ret
                (recur (unchecked-inc i) ret)))
            res))))))


(defn smoke-delim [delimited]
  (assert (= (delimited "ffdsfuiofl") "fuiof"))
  (assert (= (delimited "abbcdefg") "bb"))
  (assert (= (delimited "opoiifdopf") "poiifdop"))
  (assert (= (delimited "abcaba") "abca"))
  (assert (nil? (delimited "abc")))
  (assert (nil? (delimited "a")))
  (assert (nil? (delimited "")))
  true)




;; @steffan-westcott  beats me again!  terse and fast
(defn sw-delimited [s]
  (some->> (range (count s))
           (keep #(->> (subs s %)
                       (re-find #"^(.).*?\1")
                       first))
           not-empty
           (reduce #(max-key count %2 %1))))


(defn sw2-delimited [s]
  (some->> (range (- (count s) 2))
           (keep #(->> (subs s %)
                       (re-find #"^(.).*?\1")
                       first))
           not-empty
           (reduce #(max-key count %2 %1))))



;; @safehammand  was buggy on non-delimited, needed to add nil to max-key
;; fixed jp version of sh, still slow
(defn jp-delimited [s]
  (->> (set s)
       (mapcat #(re-seq (re-pattern (str % ".*?" %)) s))
       (reverse)
       (apply max-key count nil)))





;; hacked by SEM to fix for empty string and no delim cases
;; slow
(defn ag-delimited [s]
  (let [s-len (count s)
        delimited? (fn [s]
                     (let [middle (-> s rest butlast)
                           delim  (first s)]
                       (and (> (count s) 1)
                            (= (first s) (last s))
                            (empty? (filter #{delim} middle)))))]
    (loop [position 0
           to-take  (count s)]
      (cond
       ;; Case: Empty string
       (= to-take 0) nil
       ;; Case: We've passed the end of the string
       (< s-len (+ position to-take)) (recur 0 (dec to-take))
       ;; Case: Successfully Found
       (delimited? (->> s (drop position) (take to-take)))
           (->> s (drop position) (take to-take) (apply str))
       ;; Default Case: Advance search by 1 character
       :else (recur (inc position) to-take)))))





;; SEM hacked to fix no delim case
(defn cel-find-start-and-end [input-string]
  (let [reverse-diff (fn [[start end]] (- end start))
        get-max-tuple (partial apply max-key reverse-diff [0 0])
        indices-to-tuples (partial partition 2 1)
        group-by-chars (fn [acc [idx chr]] (update acc chr (fnil conj []) idx))]

  (->> input-string                                 ;; "wmmmcpwoqm"
       (map-indexed vector)                         ;; ([0 w] [1 m] [2 m] [3 m] [4 c] [5 p] [6 w] [7 o] [8 q] [9 m])
       (reduce group-by-chars {})                   ;; {w [0 6], m [1 2 3 9], c [4], p [5], o [7], q [8]}
       (map second)                                 ;; ([0 6] [1 2 3 9] [4] [5] [7] [8])
       (filter #(> (count %) 1))                    ;; ([0 6] [1 2 3 9])
       (map (comp get-max-tuple indices-to-tuples)) ;; ((0 6) (3 9))
       (sort-by first >)                            ;; ((3 9) (0 6))
       get-max-tuple)))                             ;; (0 6)

(defn cel-delimited [input-string]
  (let [[start end] (cel-find-start-and-end input-string)]
    (when (> end start)
      (subs input-string start (inc end)))))


;; hacked by SEM to return nil for non-delim, pretty fast
(defn mc-delimited [s]
  (->> (range (count s))
       (reduce (fn [longest i]
                 (let [substr (subs s i)
                       found-groups (re-find #"^(.)(.*?)\1" substr)
                       found (first found-groups)]
                   (if (> (count found) (count longest))
                     found
                     longest)))
               nil)
       not-empty))


;;; SEM reorganized -- now fastest with Java interop
(defn mc2-delimited [s]
  (not-empty
   (reduce (fn [longest i]
             (if-let [found (first (re-find #"^(.)(.*?)\1" (subs s i)))]
               (if (> (.length ^String found) (.length ^String longest))
                 found
                 longest)
               longest))
           ""
           (range (.length ^String s)))))


;;; still pretty fast with pure Clojure  -- basically re-invented sw-delimited!! but not
;;; nearly so fast
(defn mc42-delimited [s]
  (not-empty
   (reduce (fn [longest i] (max-key count (first (re-find #"^(.)(.*?)\1" (subs s i))) longest))
           nil
           (range (count s)))))

;; nice but not so fast
(defn mc5-delimited [s]
  (transduce (comp (map #(subs s %))
                   (map #(re-find #"^(.)(.*?)\1" %))
                   (map first))
             (completing #(max-key count %2 %1) not-empty)
             nil
             (range (count s))))

;; iterate is more work so a bit slower

(defn mc52-delimited [s]
  (transduce (comp (take (count s))
                   (map #(first (re-find #"^(.)(.*?)\1" %))))
             (completing #(max-key count %2 %1) not-empty)
             nil
             (iterate #(subs % 1) s)))

;; not bad, a bit fast than stacking up the fn with multiple map calls
(defn mc51-delimited [s]
  (transduce (map #(first (re-find #"^(.)(.*?)\1" (subs s %))))
             (completing #(max-key count %2 %1) not-empty)
             nil
             (range (count s))))

;; FASTEST of this type
;; actually takes two chars at least 
(defn mc56-delimited [s]
  (transduce (map #(first (re-find #"^(.)(.*?)\1" (subs s %))))
             (completing #(max-key count %2 %) not-empty)
             nil
             (range (- (count s) 2))))


;;; FASTEST success!
;;;  take advantage of short-circuit if current count is greater than possible width
(defn r2-delimited [s]
  (let [cnt (count s)]
    (reduce (fn [longest start]
              (let [len (count longest)]
                (if (>= len (- cnt start))
                  (reduced longest)
                  (let [found (first (re-find #"^(.)(.*?)\1" (subs s start)))]
                    (if (>= len (count found)) longest found)))))
            nil
            (range (- cnt 2)))))


(defn r2orig-delimited [s]
  (let [cnt (count s)]
    (reduce (fn [longest width]
              (let [len (count longest)]
                (if (>= len width)
                  (reduced longest)
                  (let [found (first (re-find #"^(.)(.*?)\1" (subs s (- cnt width))))]
                    (if (>= len (count found))
                      longest
                      found)))))
            nil
            (range cnt 1 -1))))








;; fastest with interop but not as pretty
(defn r3-delimited [s]
  (let [cnt (count s)]
    (not-empty
    (reduce (fn [^String longest width]
              (let [len (.length longest)]
                (if (>= len width)
                  (reduced longest)
                  (if-let [found (first (re-find #"^(.)(.*?)\1" (subs s (- cnt width))))]
                    (if (>= len (.length ^String found))
                      longest
                      found)
                    longest))))
            ""
            (range cnt 1 -1)))))
