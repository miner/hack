(ns eric.revwords)

;; https://gist.github.com/ericnormand/66a3ddaafc7c61dcb0a857aa88239142

(require '[clojure.string :as str])

(defn reverse-words [s]
  (str/join " " (rseq (str/split s #"\s+"))))

;; rseq is faster than reverse, but only works with vectors

(defn smoke-rev [reverse-words]
  (assert (= (reverse-words "my name is Eric.") "Eric. is name my"))
  (assert (= (reverse-words "hello") "hello"))
  (assert (= (reverse-words "I love you")  "you love I"))
  true)




(defn sw-reverse-words [xs]
  (->> xs (re-seq #"\S+") reverse (clojure.string/join " ")))

(defn ha-reverse-words
  [words]
  (clojure.string/join " " (reverse (clojure.string/split words #"\s+"))))


(defn atrw1 [s]
  (let [arr (.split #"\s+" s)]
    (transduce (map #(nth arr %))
               (fn ([^StringBuilder sb ^String w]
                    (if sb
                      (-> sb (.append " ") (.append w))
                      (StringBuilder. w)))
                 ([sb] (str sb)))
               nil
               (range (dec (count arr)) -1 -1))))

(defn rw1 [s]
  (let [arr (.split #"\s+" s)]
    (str (reduce (fn [^StringBuilder sb i]
                    (if sb
                      (-> sb (.append " ") (.append ^String (nth arr i)))
                      (StringBuilder. ^String (nth arr i))))
                   nil
                   (range (dec (count arr)) -1 -1)))))

;; actually fastest so far!
(defn rw22 [s]
  (let [v (str/split s #"\s+")
        cnt (count v)]
    (str (reduce (fn [^StringBuilder sb i]
                   (-> sb (.append " ") (.append ^String (v i))))
                 (StringBuilder. (if (pos? cnt) ^String (v (dec cnt)) ""))
                 (range (- cnt 2) -1 -1)))))


(defn lrw2 [s]
  (loop [ws (rseq (str/split s #"\s+")) sb nil]
    (cond (empty? ws) (str sb)
          (nil? sb) (recur (rest ws) (StringBuilder. ^String (first ws)))
          :else (recur (rest ws)
                       (-> ^StringBuilder sb (.append " ") (.append ^String (first ws)))))))

(defn lrw21 [s]
  (let [v (str/split s #"\s+")
        cnt (count v)
        init (if (pos? cnt) (v (dec cnt)) "")]
    (loop [i (- cnt 2)
           sb (StringBuilder. ^String init)]
      (if (neg? i)
        (str sb)
        (recur (dec i) (-> ^StringBuilder sb (.append " ") (.append ^String (v i))))))))


(defn rw3 [s]
  (str (reduce (fn [^StringBuilder sb ^String w]
                 (if sb
                   (-> sb (.append " ") (.append ^String w))
                   (StringBuilder. ^String w)))
               nil
               (rseq (str/split s #"\s+")))))



(defn trw1 [s]
  (transduce identity
             (completing conj #(str/join " " %))
             nil
             (.split #"\s+" s)))

;; fastest of the trans
(defn trw3 [s]
  (transduce identity
             (completing conj #(str/join " " %))
             nil
             (str/split s #"\s+")))

;; not so fast
(defn trw [s]
  (transduce identity
             (fn ([^StringBuilder sb ^String w]
                  (if sb
                    (-> sb (.insert 0 " ") (.insert 0 w))
                    (StringBuilder. w)))
               ([sb] (str sb)))
             nil
             (.split #"\s+" s)))

;; even slower
(defn trw2 [s]
  (transduce identity
             (fn ([^StringBuilder sb ^String w]
                  (if (zero? (.length sb))
                    (.append sb w)
                    (-> sb (.insert 0 " ") (.insert 0 w))))
               ([sb] (str sb)))
             (StringBuilder. (.length ^String s))
             (.split #"\s+" s)))
