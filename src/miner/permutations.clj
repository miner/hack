(ns miner.permutations)

;; SEM: NB, use math.combinatorics not this!
;; I was experimenting with writing my own permutations.

;; http://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp

#_ (defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

;; translating to Clojure
;; but recursive, non-tail, and slow
(defn all-permutations-slow [coll]
  (letfn [(all-perm
            ([coll] (all-perm coll coll))
            ([coll remain]
             (cond (nil? (seq remain)) nil
                   (nil? (next coll)) (list coll)
                   :else (concat
                          (map (fn [l] (cons (first coll) l))
                               (all-perm (rest coll)))
                          (all-perm (concat (rest coll) (list (first coll))) (rest remain))))))]
    (all-perm coll)))


;; http://stackoverflow.com/questions/13689172/better-permutations-generating-algorithm
;; (defun i-permute-quickperm (array)
;;   (let* ((len (length array))
;;          (markers (make-vector len 0))
;;          (i 1) j)
;;     (while (< i len)
;;       (if (< (aref markers i) i)
;;           (progn
;;             (setq j (if (oddp i) (aref markers i) 0))
;;             (i-swap array j i)
;;             (message "array: %s" array)
;;             (aset markers i (1+ (aref markers i)))
;;             (setq i 1))
;;         (aset markers i 0)
;;         (incf i)))))

;; rough translation, not lazy, but we can fix that
(defn rough-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (long-array len)]
    (loop [v v vs (list v) i 1]
      (if (< i len)
        (if (< (aget markers i) i)
          (let [j (if (odd? i) (aget markers i) 0)
                vj (nth v j)
                v (assoc v j (nth v i) i vj)]
            (aset markers i (inc (aget markers i)))
            (recur v (conj vs v) 1))
          (do
            (aset markers i 0)
            (recur v vs (inc i))))
        vs))))


;; See also Heap's Algorithm
;; http://en.wikipedia.org/wiki/Heap%27s_algorithm
;;
;; Note: cons is faster than conj in inner loop.
;; aset is faster than aset-int (why?)
(defn heaps-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (int-array len)]
     (loop [vs (list v) i 1]
       (if (< i len)
         (if (< (aget markers i) i)
           (let [v (first vs)
                 j (if (odd? i) (aget markers i) 0)
                 v (assoc v j (v i) i (v j))]
             (aset markers i (inc (aget markers i)))
             (recur (cons v vs) 1))
           (do
             (aset markers i 0)
             (recur vs (inc i))))
         vs))))

(defn lazy-permutations [coll]
  (lazy-seq
   (let [v (vec coll)
         len (count v)
         markers (int-array len)
         stepfn (fn step [i v]
                  (lazy-seq
                   (loop [i i v v]
                    (when (< i len)
                      (if (< (aget markers i) i)
                        (let [j (if (odd? i) (aget markers i) 0)
                              v (assoc v j (v i) i (v j))]
                          (aset markers i (inc (aget markers i)))
                          (cons v (step 1 v)))
                        (do
                          (aset markers i 0)
                          (recur (inc i) v)))))))]
     (cons v (stepfn 1 v)))))

;; loop/recur is slightly faster than recursive step at end of stepfn
(defn OK-lazy-permutations [coll]
  (lazy-seq
   (let [v (vec coll)
         len (count v)
         markers (int-array len)
         stepfn (fn step [i v]
                  (lazy-seq
                   (loop [i i v v]
                    (when (< i len)
                      (if (< (aget markers i) i)
                        (let [j (if (odd? i) (aget markers i) 0)
                              v (assoc v j (nth v i) i (nth v j))]
                          (aset markers i (inc (aget markers i)))
                          (cons v (step 1 v)))
                        (do
                          (aset markers i 0)
                          (recur (inc i) v)))))))]
     (cons v (stepfn 1 v)))))



;; ugly but it works and is lazy
(defn UGLY-lazy-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (int-array len)
        stepper (fn step [i v]
                  (lazy-seq 
                   (when (< i len)
                     (if (< (aget markers i) i)
                       (let [j (if (odd? i) (aget markers i) 0)
                             v (assoc v j (nth v i) i (nth v j))]
                         (aset markers i (inc (aget markers i)))
                         (cons v (step 1 v)))
                       (do
                         (aset markers i 0)
                         (step (inc i) v))))))]
    (lazy-seq (cons v (stepper 1 v)))))

;; wrong idea was to just wrap a lazy-seq around quick-perm loop
;; looked OK at first for take, but only because take is lazy itself
;; as soon as you needed anything, it forced the full loop to run -- not really lazy


;; SEM: why is aset faster than aset-long?  Extra checking?



;;;;;;;;;; borrowed from halfbaked
(def demangle-replacements
  (array-map "_QMARK_" "?"
             "_BANG_" "!"
             "_STAR_" "*"
             "_GT_" ">"
             "_EQ_" "="
             "_PLUS_" "+"
             "_LT_" "<"
             "_SLASH_" "/"
             "_AMPERSAND_" "&"
             "_TILDE_" "~"
             ;; keep underbar last
             "_" "-"))

;; a faster but ugly version is in demangle.clj
(defn ^String demangle
  "Demangle a clojure identifier name"
  [^String s]
  (reduce-kv str/replace s demangle-replacements))

(defn compiled-fn-name
  "returns the simple name (a string) for the given function f as determined by the compiler"
  [f]
  (let [f (if (var? f) (var-get f) f)
        compiled-name (when (fn? f) (str f))
        fname (second (first (re-seq #"[$](.*)@" compiled-name)))]
    (if fname
      (demangle fname)
      compiled-name)))


(defn test-perm
  ([] (test-perm lazy-permutations))
  ([pfn] (test-perm 9 pfn))
  ([n pfn]
  (let [v (vec (range n))
        trials 3]
    (when (= (set (combo/permutations v)) (set (pfn v)))
      (println "combo count")
      (dotimes [_ trials]
        (time (count (combo/permutations v))))
      (println (compiled-fn-name pfn) "count")
      (dotimes [_ trials]
        (time (count (pfn v))))
      (println "combo add take 3")
      (dotimes [_ trials]
        (time (reduce + (apply concat (take 3 (combo/permutations v))))))
      (println (compiled-fn-name pfn) "add take 3")
      (dotimes [_ trials]
        (time (reduce + (apply concat (take 3 (pfn v))))))))))
      
