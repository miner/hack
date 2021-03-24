(ns miner.estrdiff)

;; https://gist.github.com/ericnormand/f0df145ef62eacb0d7becd1b6d5e3d28

;;; String difference

;;; You are given two strings, a and b. We consider each letter to be unique, meaning
;;; duplicates are significant. Write a function that returns the count of letters in b
;;; which do not occur in a.


;; Works
(defn strdiff [a b]
  (reduce (fn [m c]
            (if-let [n (get m c)]
              (if (= n 1)
                (dissoc m c)
                (assoc m c (dec n)))
              m))
          (frequencies b)
          (seq a)))


;; not faster
(defn sd4 [a b]
  (reduce (fn [m c]
            (let [n (get m c)]
              (cond (nil? n) m
                    (= n 1) (dissoc m c)
                    :else (assoc m c (dec n)))))
          (frequencies b)
          (seq a)))


;; slightly faster with transients
(defn tstrdiff [a b]
  (persistent!
   (reduce (fn [m c]
             (if-let [n (get m c)]
               (if (= n 1)
                 (dissoc! m c)
                 (assoc! m c (dec n)))
               m))
           (transient (frequencies b))
           (seq a))))

;; slow to merge and select
;; [deleted]




(defmacro assert=
  ([] true)
  ([form result]
   `(do (assert (= ~form ~result)) true))
  ([form result & more]
   `(and (assert= ~form ~result)
         (assert= ~@more))))


(defn smoke-sd [strdiff]
  (assert= (strdiff "abcd" "dec")  {\e 1}
           (strdiff "abc" "aabc") {\a 1}
           (strdiff "abc" "") {}
           (strdiff "abc" "abc") {}
           (strdiff "" "abc") {\a 1 \b 1 \c 1}
           (strdiff "axx" "abcc") {\b 1 \c 2}
           (strdiff "xxxx" "xxxxxx") {\x 2}))


;; not so fast
(defn sw-strdiff [a b]
  (let [[fa fb] (map frequencies [a b])
        diff-freqs (reduce-kv (fn [m ch freq] (update m ch (fnil - 0) freq)) fb fa)]
    (into {} (filter (comp pos? val) diff-freqs))))


;; @arthurulacerda   fast and short -- the winner so far
(defn al-strdiff
  [s1 s2]
  (frequencies (reduce #(clojure.string/replace-first %1 %2 "") s2 s1)))





;;;; My older attempts...
;; SEM BUG: you need to consider any position, not exact matches only.  You were doing it by
;; place.  Revised test to show different examples.  These worked on original tests.


(defn strdiff1-bad [a b]
  (let [lena (count a)
        lenb (count b)
        extras (when (> lenb lena) (seq (subs b lena)))]
    (frequencies (concat (remove nil? (map (fn [x y] (when (not= x y) x)) b a))
                         extras))))

(defn sdiff-bad [a b]
  (frequencies (for [i (range (count b))
                     :let [bi (nth b i)]
                     :when (not= (nth a i nil) bi)]
                 bi)))


;; submitted version, then deleted
(defn strdiff-bad [a b]
  (reduce-kv (fn [m i c] (if (= c (nth a i nil)) m (assoc m c (inc (get m c 0)))))
             {}
             (vec b)))

;; fortunately, didn't post version with extended protocol to support IKVReduce on String


