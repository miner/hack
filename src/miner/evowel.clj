(ns miner.evowel)

;; https://gist.github.com/ericnormand/9e5782005bff705d61444ef8bb6e15df

;; Distance to nearest vowel.  Write a function that takes a string as argument. Each
;; character in the string will be a letter. The function should return a sequence
;; containing the distances from each corresponding letter in the string to the nearest
;; vowel in the string.


;; new fastest, self-contained version, single pass
(defn nvows [string]
  (let [into! (fn [tv coll] (reduce conj! tv coll))
        cnt (.length ^String string)
        find-vow (fn [i]
                   (when (< i cnt)
                     (case (.charAt ^String string i)
                       (\a \e \i \o \u \A \E \I \O \U) i
                       (recur (inc i)))))]
    (when-let [z0 (find-vow 0)]
      (loop [z1 z0
             z2 (find-vow (inc z0))
             res (into! (transient []) (range z0 0 -1))]
        (if z2
          (let [d (- z2 z1)
                h (quot (inc d) 2)]
            (recur z2 (find-vow (inc z2))
                   (-> res (into! (range h)) (into! (range (- d h) 0 -1)))))
          (persistent! (into! res (range (- cnt z1)))))))))



(defn mvows [string]
  (let [cnt (count string)
        vow? (fn [i]
               (case (nth string i)
                 (\a \e \i \o \u \A \E \I \O \U) true
                 false))]
    (reduce (fn [res i]
              (let [z (:z (meta res))]
                (cond (= i cnt) (into res (range (- cnt z)))
                      (vow? i) (if z
                                 (let [d (- i z)
                                       h (quot (inc d) 2)]
                                   (-> res
                                       (into (range h))
                                       (into (range (- d h) 0 -1))
                                       (with-meta {:z i})))
                                 (with-meta (vec (range i 0 -1)) {:z i}))
                      :else res)))
            []
            (range (inc cnt)))))



;;; sem state should be previous vowel index -- might be reinventing
;;;

(defn rvows [string]
  (let [cnt (count string)
        vow? (fn [i]
               (case (nth string i)
                 (\a \e \i \o \u \A \E \I \O \U) true
                 false))]
    (reduce (fn [res i]
              (let [z (peek res)
                    r (pop res)]
                (cond (= i cnt) (into r (range (- cnt z)))
                      (vow? i) (if z
                                 (let [d (- i z)
                                       h (quot (inc d) 2)]
                                   (-> r 
                                       (into (range h))
                                       (into (range (- d h) 0 -1))
                                       (conj i)))
                                 (conj (vec (range i 0 -1)) i))
                      :else res)))
            [nil]
            (range (inc cnt)))))

;; minimal speed improvement
(defn rvows2 [string]
  (let [cnt (.length ^String string)
        into! (fn [tv coll] (reduce conj! tv coll))
        peek! (fn [tv] (nth tv (dec (count tv)) nil))
        vow? (fn [i]
               (case (.charAt ^String string i)
                 (\a \e \i \o \u \A \E \I \O \U) true
                 false))]
    (reduce (fn [res i]
              (let [z (peek! res)
                    r (pop! res)]
                (cond (= i cnt) (persistent! (into! r (range (- cnt z))))
                      (vow? i) (if z
                                 (let [d (- i z)
                                       h (quot (inc d) 2)]
                                   (-> r 
                                       (into! (range h))
                                       (into! (range (- d h) 0 -1))
                                       (conj! i)))
                                 (-> (transient [])
                                     (into! (range i 0 -1)) 
                                     (conj! i)))
                      :else (conj! r z))))
            (transient [nil])
            (range (inc cnt)))))


                
;; no Java interop, slightly slower
(defn nvows3 [string]
  (let [into! (fn [tv coll] (reduce conj! tv coll))
        cnt (count string)
        find-vow (fn [i]
                   (case (nth string i nil)
                     nil nil
                     (\a \e \i \o \u \A \E \I \O \U) i
                     (recur (inc i))))]
    (when-let [z0 (find-vow 0)]
      (loop [z1 z0
             z2 (find-vow (inc z0))
             res (into! (transient []) (range z0 0 -1))]
        (if z2
          (let [d (- z2 z1)
                h (quot (inc d) 2)]
            (recur z2 (find-vow (inc z2))
                   (-> res (into! (range h)) (into! (range (- d h) 0 -1)))))
          (persistent! (into! res (range (- cnt z1)))))))))


;; not faster to use -1 for missing vowel
;; not faster using bit-shift instead of quot/2



;;; SEM: make reduce-kv work on a String
;;; extent the IKVReduce protocol
;;;  use the .charAt

;; (reduce-kv conj [] "Foo")
;; Execution error (IllegalArgumentException) at user/eval7443 (REPL:1).
;; No implementation of method: :kv-reduce of protocol: #'clojure.core.protocols/IKVReduce found for class: java.lang.String
;; user=> (reduce-kv conj [] (vec "Foo"))
;; [0 \F 1 \o 2 \o]
;; 


(defn vowel1? [ch]
  (contains? #{\a \e \i \o \u \A \E \I \O \U} ch))

;; case is faster than set notation
;; probably good to have capital letters in set rather than force lowercase
(defn vowel? [ch]
  (case ch
    (\a \e \i \o \u \A \E \I \O \U) true
    false))

(defmacro ch-in-fn [chars]
  (let [chars (seq chars)]
    `(fn [ch#]
       (case ch#
         ~chars true
         false))))

(def vow? (ch-in-fn "aeiouAEIOU"))


;; I believe this is slightly faster than Math/abs probably because of type dispatch.
(defn dist [i j]
  (let [d (- i j)]
    (if (neg? d) (- d) d)))

;; but slowish -- too many distance checks of all vowels, should only check before and after i
(defn simple-nearest-vowels [string]
  (let [vowel? (fn [ch] (case ch (\a \e \i \o \u \A \E \I \O \U) true  false))
        dist (fn [i j] (let [d (- i j)] (if (neg? d) (- d) d)))
        vis (keep-indexed (fn [i c] (when (vowel? c) i)) string)]
    (mapv (fn [i] (apply min (map #(dist i %) vis))) (range (count string)))))


;; second best, but transients faster below
(defn slvowels [string]
  (let [cnt (.length ^String string)]
    (loop [vis (keep (fn [i] (when (vowel? (.charAt ^String string i)) i)) (range cnt))
           res (vec (range (first vis) 0 -1))]
      (let [z1 (first vis)]
        (if-let [z2 (second vis)]
          (let [d (- z2 z1)
                w (quot (inc d) 2)]
            (recur (rest vis) (into (into res (range w)) (range (- d w) 0 -1))))
          (into res (range (- cnt z1))))))))


(defn into! [tv coll]
  (reduce conj! tv coll))

(defn slvowels3 [string]
  (let [cnt (.length ^String string)]
    (loop [vis (keep (fn [i] (when (vowel? (.charAt ^String string i)) i)) (range cnt))
           res (into! (transient []) (range (first vis) 0 -1))]
      (let [z1 (first vis)]
        (if-let [z2 (second vis)]
          (let [d (- z2 z1)
                w (quot (inc d) 2)]
            (recur (rest vis) (into! (into! res (range w)) (range (- d w) 0 -1))))
          (persistent! (into! res (range (- cnt z1)))))))))




(defn rzvowels [string]
  (let [cnt (count string)
        vlr (pop (reduce-kv (fn [rz i c]
                              (let [r (pop rz)
                                    z (peek rz)]
                                (if (vowel? c)
                                  (conj r 0 i)
                                  (conj r (dist i z) z))))
                            [cnt]
                            (vec string)))]
    
    (pop (reduce (fn [rz i]
                   (let [z (peek rz)
                         r (pop rz)
                         x (vlr i)]
                      (if (zero? x)
                        (conj r i)
                        (let [d (dist i z)]
                          (if (< d x)
                            (conj (assoc r i d) z)
                            rz)))))
                  (conj vlr -1)
                  (range (dec cnt) -1 -1)))))


;; was almost fastest with loops and transient
;; but more complicated during reverse pass
(defn tlvowels [string]
  (let [cnt (.length ^String string)]
    (loop [r (transient []) i 0 z cnt]
      (if (< i cnt)
        (if (vowel? (.charAt ^String string i))
          (recur (conj! r 0) (inc i) i)
          (recur (conj! r (dist i z)) (inc i) z))
        (loop [r r i (dec z) z z]
          (if (neg? i)
            (persistent! r)
            (let [x (r i)]
              (if (zero? x)
                (recur r (dec i) i)
                (let [d (dist i z)]
                  (if (< d x)
                    (recur (assoc! r i d) (dec i) z)
                    (recur r (dec i) z)))))))))))



(defn smoke-vowel [nearest-vowels]
  (assert (= (nearest-vowels "aeiou") [0 0 0 0 0]))
  (assert (= (nearest-vowels "babbb") [1 0 1 2 3]))
  (assert (= (nearest-vowels "babbba") [1 0 1 2 1 0]))
  (assert (= (nearest-vowels "babbbba") [1 0 1 2 2 1 0]))
  (assert (= (nearest-vowels "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba")
             [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 17 16 15
              14 13 12 11 10 9 8 7 6 5 4 3 2 1 0]))
  (assert (= (nearest-vowels "abbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbx")
             [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
              21 22 23 24 25 26 27 28 29 30 31 32 33 34 35]))
  (assert (= (nearest-vowels "xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba")
             [35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18
              17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0]))
  (assert (= (nearest-vowels "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
  true)




;; nin... solution, slightly hacked, still very slow
(defn nin-nvs [s]
  (let [cnt (count s)
        distances (fn [i]
                    (concat (range i 0 -1)
                            (range (- cnt i))))]
    (apply map min
           (keep-indexed (fn [i c]
                           (when (#{\a \i \e \o \u \A \I \E \O \U} c)
                             (distances i)))
                         s))))



;; Eric Normand (slightly changed by SEM)
(defn en-nearest-vowels [s]
  (let [vowel-indexes
        (->> s
             (clojure.string/lower-case)
             (map-indexed vector)
             (filter #(#{\a \e \i \o \u} (second %)))
             (map first))]
    (map-indexed (fn [i _]
                   (->> vowel-indexes
                        (map #(Math/abs ^long (- i %)))
                        (apply min)))
                 s)))


;; G7S, hacked for caps
(defn orig-g7s-nearest-vowels
  [s]
  (reduce
   (fn [acc c]
     (let [head (or (peek acc) 0)]
      (if (contains? #{\a \i \e \o \u \A \I \E \O \U} c)
        (let [back (int (if (= head (count acc)) head (/ head 2)))]
          (into (subvec acc 0 (- (count acc) back))
                (range back -1 -1)))
        (conj acc (inc head)))))
   []
   s))



;; SEM hacked for caps and simplified head with nil-punning (initial head is special anyway)
;; g7s key insight about subvec for backtracking is still the core idea, but the initial
;; build up is stack of nils so head as nil is also marker for initial bit.  case expr is
;; faster than set.  Better than 25% speed improvement over orig g7s.  I like the look of
;; this one.  Thumbs up!
(defn g7s-nvs
  [s]
  (reduce
   (fn [acc c]
     (let [head (peek acc)]
       (if (case c (\a \i \e \o \u \A \I \E \O \U) true false)
         (if head
           (let [back (quot head 2)]
             (into (subvec acc 0 (- (count acc) back))
                   (range back -1 -1)))
           (vec (range (count acc) -1 -1)))
        (conj acc (and head (inc head))))))
   []
   s))


;;; experiments with transients didn't really improve g7s.  Subvec is very efficient.
;;; Transients don't allow subvec and the hack-arounds aren't worth it.




;; SEM my other stuff

(defn seg-vowels [string]
  (let [cnt (count string)
        vis (keep-indexed (fn [i c] (when (vowel? c) i)) string)
        segments (into [[nil (first vis)]] (partition-all 2 1 vis))]
    (mapcat (fn [[z1 z2]]
              (cond (nil? z1) (range z2 0 -1)
                    (nil? z2) (range (- cnt z1))
                    :else (let [d (- z2 z1)
                                w (quot (inc d) 2)]
                            (concat (range w) (range (- d w) 0 -1)))))
            segments)))
                  
(defn svowels [string]
  (let [cnt (count string)
        vis (keep-indexed (fn [i c] (when (vowel? c) i)) string)]
    (mapcat (fn [[z1 z2]]
              (cond (nil? z1) (range z2 0 -1)
                    (nil? z2) (range (- cnt z1))
                    :else (let [d (- z2 z1)
                                w (quot (inc d) 2)]
                            (concat (range w) (range (- d w) 0 -1)))))
            (map list (cons nil vis) (concat vis (list nil))))))


(defn svowels2 [string]
  (let [cnt (count string)]
    (mapcat (fn [[z1 z2]]
              (cond (nil? z1) (range z2 0 -1)
                    (nil? z2) (range (- cnt z1))
                    :else (let [d (- z2 z1)
                                w (quot (inc d) 2)]
                            (concat (range w) (range (- d w) 0 -1)))))
            (partition-all 2 1 (cons nil (keep-indexed (fn [i c] (when (vowel? c) i)) string))))))



(defn svowels3 [string]
  (let [cnt (count string)]
    (mapcat (fn [[z1 z2]]
              (cond (nil? z1) (range z2 0 -1)
                    (nil? z2) (range (- cnt z1))
                    :else (let [d (- z2 z1)
                                w (quot (inc d) 2)]
                            (concat (range w) (range (- d w) 0 -1)))))
            (partition-all 2
                           (cons nil
                                 (mapcat (fn [i c] (when (vowel? c) (list i i)))
                                         (range)
                                         string))))))




(defn slvowels1 [string]
  (loop [vis (keep-indexed (fn [i c] (when (vowel? c) i)) string)
         res (vec (range (first vis) 0 -1))]
    (let [z1 (first vis)]
      (if-let [z2 (second vis)]
        (let [d (- z2 z1)
              w (quot (inc d) 2)]
          (recur (rest vis) (into (into res (range w)) (range (- d w) 0 -1))))
        (into res (range (- (count string) z1)))))))




(defn zvowels [string]
  (let [cnt (count string)
        vis (keep-indexed (fn [i c] (when (vowel? c) i)) string)]
    (reduce (fn [r z]
              (map-indexed (fn [i x]
                             (cond (nil? x) (dist i z)
                                   (zero? x) 0
                                   :else (min x (dist i z))))
                           r))
            (repeat cnt nil)
            vis)))


(defn nvowels [string]
  (let [cnt (count string)
        pass0 (mapv (fn [c] (if (vowel? c) 0 cnt)) string)
        pass1 (pop (reduce-kv (fn [rz i x]
                                (let [r (pop rz)
                                      z (peek rz)]
                                  (if (zero? x)
                                    (conj r 0 i)
                                    (let [d (dist i z)]
                                      (if (< d x)
                                        (conj r d z)
                                        (conj r x z))))))
                              [cnt]
                              pass0))]
    (pop (reduce (fn [rz i]
                   (let [z (peek rz)
                         r (pop rz)
                         x (pass1 i)]
                     (if (zero? x)
                       (conj r i)
                       (let [d (dist i z)]
                         (if (< d x)
                           (conj (assoc r i d) z)
                           rz)))))
                 (conj pass1 -1)
                 (range (dec cnt) -1 -1)))))



(defn cvowels [string]
  (let [cnt (count string)
        vlr (pop (reduce (fn [rz i]
                           (let [c (.charAt ^String string ^long i)
                                 r (pop rz)
                                 z (peek rz)]
                             (if (vowel? c)
                               (conj r 0 i)
                               (conj r (dist i z) z))))
                         [cnt]
                         (range cnt)))]
    (pop (reduce (fn [rz i]
                   (let [z (peek rz)
                         r (pop rz)
                         x (vlr i)]
                     (if (zero? x)
                       (conj r x i)
                       (let [d (dist i z)]
                         (if (< d x)
                           (conj r d z)
                           (conj r x z))))))
                 '(-1)
                 (range (dec cnt) -1 -1)))))

