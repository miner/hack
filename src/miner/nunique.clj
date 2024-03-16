(ns miner.nunique)

;;; find first substring of length N within string with all unique characters (within that
;;; substring)

;;; https://clojure-diary.gitlab.io/2024/03/12/better-sieve-finding-first-n-unique-characters-in-clojure.html

(def string "rgaraga4agjrj4rikllmrfmghjqwwrwengek")

(defn sieve [length coll]
  (->> coll
       (partition length 1)
       (some #(when (apply distinct? %)
                (apply str %)))))

(defn test-sieve [sfn]
  (let [rrr "rgaraga4agjrj4rikllmrfmghjqwwrwengek"
        aaa "AAAAAAAAAAAAAAAAAAA"
        abc "abcdefabcdefabcdefabcdefabcdefabcdefabcdefabcdefg"]
  (assert (= (sfn 4 rrr) "4agj"))
  (assert (nil? (sfn 4 aaa)))
  (assert (= (sfn 7 abc) "abcdefg"))
  (assert (nil? (sfn 8 abc)))
  true))

;;; borrowed from transmuters

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; q clojure.lang.PersistentQueue
(defn push
  "Push x onto PersistentQueue q, popping q as necessary to keep count <= limit"
  ([q x] (conj q x))
  ([q limit x]
   ;; {:pre [(pos? limit)]}
   (if (>= (count q) limit)
     (recur (pop q) limit x)
     (conj q x))))

(defn slide
  ([n] (slide n []))
  ([n init]
   (fn [rf]
     (let [qv (volatile! (queue init))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [q (vswap! qv push n input)]
            (if (< (count q) n)
              result
              (rf result (seq q))))))))))

(defn uniq? [coll]
  (apply distinct? coll))

(defn semsub1 [width s]
  (when-let [v (first (sequence (comp (slide width) (filter uniq?) (take 1)) s))]
    (apply str v)))
             

(defn semsub2 [width s]
  (first (filter uniq? (map #(subs s % (+ width %)) (range (- (inc (count s)) width))))))


;;; fastest
(defn semsub [width s]
  (first (sequence (comp (map #(subs s % (+ width %))) (filter uniq?) (take 1))
                   (range (- (inc (count s)) width)))))




;;; double time
(defn ss3 [width s]
  (let [unique-at? (fn [i]
                     (reduce (fn [r j]
                               (let [c (String/charAt s j)]
                                 (if (contains? r c)
                                   (reduced nil)
                                   (conj r c))))
                             (hash-set (String/charAt s i))
                             (range (inc i) (+ i width))))]
    (when-let [start (first (filter unique-at? (range (- (inc (count s)) width))))]
      (subs s start (+ start width)))))


;;; slightly faster
(defn ss4 [width s]
  (let [result (reduce (fn [res i]
                         (let [r (conj res (String/charAt s i))]
                           (if (apply distinct? r)
                             (reduced (apply str r))
                             (pop r))))
                       (queue (seq (subs s 0 (dec width))))
                       (range width (count s)))]
    (when (string? result)
      result)))


;;; must uniq all 4 or mark pre-duplicate
;;; probably should skip past dups
(defn BUG-ss5 [width s]
  (let [result (reduce (fn [res i]
                         (let [c (String/charAt s i)]
                           (if (some #(= ^char c ^char %) res) ;;;BUG
                             (conj (subvec res 1) c)
                             (reduced (apply str (conj res c))))))
                       (vec (subs s 0 (dec width)))
                       (range width (count s)))]
    (when (string? result)
      result)))

;; "unique conj" drops head up to duplicate before adding
;; v is expected to be small so it doesn't hurt to linear search
(defn uconj [v c]
  (loop [i (dec (count v))]
    (cond (neg? i) (conj v c)
          (= (v i) c) (conj (subvec v (inc i)) c)
          :else (recur (dec i)))))

;;; depends on width being relatively small, like 4, might not scale well
;;; was fastest
(defn ss6 [width s]
  (let [res (reduce (fn [uv c]
                      (let [v (uconj uv c)]
                        (if (= (count v) width)
                          (reduced (apply str v))
                          v)))
                    []
                    s)]
    (when (string? res)
      res)))

;;; pretty much same as above, but prettier
;; conj is the no-op transducer, faster than (map identity)
(defn ss7 [width s]
  (transduce conj  
             (fn ([uv c]
                  (let [v (uconj uv c)]
                    (if (= (count v) width)
                      (reduced (apply str v))
                      v)))
               ([res] (when (string? res) res)))
             []
             s))


;;; integrated unconj
;;; new fastest, and compact
(defn ss8 [width s]
  (transduce conj  
             (fn ([v c]
                  (loop [i (dec (count v))]
                    (cond (neg? i) (let [uv (conj v c)]
                                     (if (= (count uv) width)
                                       (reduced (apply str uv))
                                       uv))
                          (= (v i) c) (conj (subvec v (inc i)) c)
                          :else (recur (dec i)))))
               ([res] (when (string? res) res)))
             []
             s))


;;; slower and more confusing
(defn ss9 [width s]
  (transduce conj  
             (fn ([v c]
                  (reduce (fn [v i]
                            (cond (neg? i) (let [uv (conj v c)]
                                             (if (= (count uv) width)
                                               (reduced (reduced (apply str uv)))
                                               uv))
                                  (= (v i) c) (reduced (conj (subvec v (inc i)) c))
                                  :else v))
                          v
                          (range (dec (count v)) -2 -1)))
               ([res] (when (string? res) res)))
             []
             s))


;;; much faster!  new fastest
(defn ssrkv [width s]
  (let [res (reduce-kv (fn [st i c]
                         (loop [j (dec i)]
                           (cond (< j st) (if (= (- i st) (dec width))
                                            (reduced (subs s st (inc i)))
                                            st)
                                 (= (String/charAt s j) ^char c) (inc j)
                                 :else (recur (dec j)))))
                       0
                       (vec s))]
    (when (string? res)
      res)))
    



;;; Super fastest.   New champion  -- uses Clojure 1.12-alpha9 method
(defn uniq-subs [width s]
  (let [res (reduce (fn [st i]
                      (let [c (String/charAt s i)]
                        (loop [j (dec i)]
                          (cond (< j st) (if (= (- i st) (dec width))
                                           (reduced (subs s st (inc i)))
                                           st)
                                (= (String/charAt s j) c) (inc j)
                                :else (recur (dec j))))))
                    0
                    (range 1 (count s)))]
    (when (string? res)
      res)))


;;; about same, maybe nicer
(defn xuniq-subs [width s]
  (transduce conj
             (fn ([st i]
                  (let [c (String/charAt s i)]
                    (loop [j (dec i)]
                      (cond (< j st) (if (= (- i st) (dec width))
                                       (reduced (subs s st (inc i)))
                                       st)
                            (= (String/charAt s j) c) (inc j)
                            :else (recur (dec j))))))
               ([res] (when (string? res) res)))
             0
             (range 1 (count s))))


;;; old method notation
(defn zuniq-subs [width s]
  (transduce conj
             (fn ([st i]
                  (let [c (.charAt ^String s ^int i)]
                    (loop [j (dec i)]
                      (cond (< j st) (if (= (- i st) (dec width))
                                       (reduced (subs s st (inc i)))
                                       st)
                            (= (.charAt ^String s ^int j) c) (inc j)
                            :else (recur (dec j))))))
               ([res] (when (string? res) res)))
             0
             (range 1 (count s))))


;;; file bug ask/clojure about reduce-kv on strings.  Probably easy fix by adding something
;;; for IKVReduce

(when-not (extends? clojure.core.protocols/IKVReduce java.lang.String)
  (extend-type java.lang.String clojure.core.protocols/IKVReduce
    (kv-reduce [s f init]
      (reduce (fn [res i] (f res i (String/charAt s i))) init (range (count s))))))


(defn urkv-subs [width s]
  (let [res (reduce-kv (fn [st i c]
                         (loop [j (dec i)]
                           (cond (< j st) (if (= (- i st) (dec width))
                                            (reduced (subs s st (inc i)))
                                            st)
                                 (= (String/charAt s j) ^char c) (inc j)
                                 :else (recur (dec j)))))
                       0
                       s)]
    (when (string? res)
      res)))

;;; not sure this is good idea
(defmacro when-> [expr & preds]
  (let [name (gensym)]
    `(as-> ~expr ~name
       ~@(map (fn [p] `(when (and ~name (~p ~name)) ~name)) preds))))
  

  
