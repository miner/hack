(ns miner.ebigram)


;;; https://gist.github.com/ericnormand/724c98a2ff399c82469ce2dd1ea3e23c

;;; A bigram is a sequence of two letters. Common bigrams in English include st and
;;; tr. Write a function to check if all bigrams in a collection are present in the words in
;;; a string.

;;; SEM: obvious way is
(defn all-present? [bigrams s]
  (every? #(clojure.string/includes? s %) bigrams))




(defn smoke-bigrams [all-present?]
  (assert (all-present? ["st" "tr"] "street"))
  (assert (not (all-present? ["ea" "ng" "kt"] "eating at a restaurant")))
  (assert (all-present? [] "hello!"))
  true)


;;; Now, let's try a crazy way to do this...
;;; but very slow compared to obvious



;; bi is a string bigram, c and d are chars
;; assume all are lowercase

;; crazy slow
(defn allbi2? [bigrams s]
  (let [two-bytes (fn [[c d]] (+ (byte c) (* 256 (byte d))))
        byset (into #{} (map two-bytes) (partition 2 1 s))]
    (every? byset (map two-bytes bigrams))))


;; crazy slow
(defn allbi3? [bigrams s]
  (let [two-bytes (fn [[c d]] (bit-or (byte c) (bit-shift-left (byte d) 8)))
        byset (into #{} (map two-bytes) (partition 2 1 s))]
    (every? byset (map two-bytes bigrams))))



(defn ascii [^String s]
  (.getBytes s "ASCII"))


;; crazy slow (30+ us)
(defn allbi4? [bigrams s]
  (let [two-bytes (fn [[c d]] (+ (* 256 c) d))
        byset (into #{} (map two-bytes) (partition 2 1 (.getBytes ^String s "ASCII")))]
    (every? byset (map #(two-bytes (.getBytes ^String % "ASCII")) bigrams))))



(defn bytepairs [^String s]
  (let [bs (.getBytes s "ASCII")]
    (into #{} (map (fn [a b] (+ (* 256 a) b)) bs (rest bs)))))


;; somewhat better (13 us)
(defn allbi5? [bigrams s]
  (let [two-bytes (fn [[c d]] (+ (* 256 c) d))
        bs (.getBytes ^String s "ASCII")
        byset (into #{} (map (fn [a b] (+ (* 256 a) b)) bs (rest bs)))]
    (every? byset (map #(two-bytes (.getBytes ^String % "ASCII")) bigrams))))

(defn allbi6? [bigrams s]
  (let [two-bytes (fn [^bytes cd] (+ (* 256 (aget cd 0)) (aget cd 1)))
        bs (.getBytes ^String s "ASCII")
        byset (into #{} (map (fn [a b] (+ (* 256 a) b)) bs (rest bs)))]
    (every? byset (map #(two-bytes (.getBytes ^String % "ASCII")) bigrams))))

;; slower with sequence


;; pretty good but still slower than the obvious (15x)
;; (6 us)
(defn allbi8? [bigrams s]
  (let [two-bytes (fn [^bytes cd] (+ (* 256 (aget cd 0)) (aget cd 1)))
        bs (.getBytes ^String s "ASCII")
        byset (reduce (fn [res i]
                        (conj res (+ (* 256 (aget bs i)) (aget bs (inc i)))))
                      #{}
                      (range (dec (count bs))))]
    (transduce (comp (map #(.getBytes ^String % "ASCII")) (map two-bytes))
               (fn ([r x] (or (byset x) (reduced false)))
                 ([r] (boolean r)))
               true
               bigrams)))

;;; assumes case-sensitive -- probably not a good idea
;;; if you want case-insensitive you need to lowercase everything first


;; fastest of my stupid ways but still slow than the obvious (6x)
;; (3 us)
(defn allbi9? [bigrams s]
  (let [ascii (fn [s] (.getBytes ^String s "ASCII"))
        two-bytes (fn [^bytes ba offset]
                    (bit-or (bit-shift-left (aget ba offset) 8) (aget ba (inc offset))))
        bigram (fn [s] (two-bytes (ascii s) 0))
        bs ^bytes (ascii s)]
    (transduce (map (fn [i] (two-bytes bs i)))
               (fn ([r x]
                    (let [r (disj! r x)]
                      (if (zero? (count r))
                        (reduced nil)
                        r)))
                 ([r] (nil? r)))
               (transduce (map bigram) conj! (transient #{})  bigrams)
               (range (dec (alength bs))))))
