(ns miner.ergb
  (:require [clojure.edn :as edn]))

;; https://gist.github.com/ericnormand/b93b4f9cc9ab0bd5d396c9dac8bcfd7d

;; Your task is to take a collection of RGB strings of the form "#FF021A", mix them like the
;; algorithm above, and return the resulting RGB string.  Note that each RGB string contains
;; two hexadecimal digits for each component. You can round the averages to integers however
;; you want.


;; Long/parseLong (or Integer/parseInt) is faster than edn/read-string

(defn parse-rgb [rgbstr]
  (let [rgb (Long/parseLong (subs rgbstr 1) 16)
        r (unsigned-bit-shift-right rgb 16)
        g (bit-and 0xFF (unsigned-bit-shift-right rgb 8))
        b (bit-and 0xFF rgb)]
    [r g b]))

(defn rgb-mix [rgbs]
  (let [cs (map parse-rgb rgbs)
        cnt (count cs)
        avg-nth (fn [n] (long (/ (reduce #(+ % (nth %2 n)) 0.5 cs) cnt)))]
    (format "#%02X%02X%02X" (avg-nth 0) (avg-nth 1) (avg-nth 2))))

(defn rgb55 [rgbs]
  (let [cs (map parse-rgb rgbs)
        cnt (count cs)
        avg-nth (fn [n] (long (quot (reduce #(+ % (nth %2 n)) 0.5 cs) cnt)))]
    (format "#%02X%02X%02X" (avg-nth 0) (avg-nth 1) (avg-nth 2))))


(defn rgb56 [rgbs]
  (let [cs (map parse-rgb rgbs)
        cnt (count cs)
        avg-nth (fn [n] (unchecked-divide-int (long (reduce #(+ % (nth %2 n)) 0.5 cs)) cnt))]
    (format "#%02X%02X%02X" (avg-nth 0) (avg-nth 1) (avg-nth 2))))

;; pretty fast
(defn rgb57 [rgbs]
  (let [cs (map parse-rgb rgbs)
        cnt (count cs)
        avg-nth (fn [n] (unchecked-divide-int (reduce #(+ % (nth %2 n)) 0 cs) cnt))]
    (format "#%02X%02X%02X" (avg-nth 0) (avg-nth 1) (avg-nth 2))))


(defn list-rgb [rgbstr]
  (let [rgb (Long/parseLong (subs rgbstr 1) 16)
        r (unsigned-bit-shift-right rgb 16)
        g (bit-and 0xFF (unsigned-bit-shift-right rgb 8))
        b (bit-and 0xFF rgb)]
    (list r g b)))


;; not so fast, but looks good
(defn rgb61 [rgbs]
  (->> rgbs
      (map parse-rgb)
      (reduce #(mapv + % %2))
      (mapv #(quot % (count rgbs)))
      (apply format "#%02X%02X%02X")))

(defn rgb62 [rgbs]
  (->> rgbs
      (map parse-rgb)
      (reduce #(map + % %2))
      (map #(quot % (count rgbs)))
      (apply format "#%02X%02X%02X")))

(defn rgb63 [rgbs]
  (->> rgbs
      (map list-rgb)
      (reduce #(map + % %2))
      (map #(quot % (count rgbs)))
      (apply format "#%02X%02X%02X")))



(defn rgb-bytes0 [rgbstr]
  (let [r (Integer/parseInt (subs rgbstr 1 3) 16)
        g (Integer/parseInt (subs rgbstr 3 5) 16)
        b (Integer/parseInt (subs rgbstr 5 7) 16)]
    (list r g b)))

(defn rgb-bytes [rgbstr]
  (map (fn [i] (Integer/parseInt (subs rgbstr i (+ i 2)) 16)) [1 3 5]))


;; not so fast, but compact
(defn rgb73 [rgbs]
  (->> rgbs
      (map #(map (fn [i] (Integer/parseInt (subs % i (+ i 2)) 16)) [1 3 5]))
      (reduce #(map + % %2))
      (map #(quot % (count rgbs)))
      (apply format "#%02X%02X%02X")))

;; not fast, but bonus for transducing
(defn rgb83 [rgbs]
  (transduce
   (map #(map (fn [i] (Integer/parseInt (subs % i (+ i 2)) 16)) [1 3 5]))
   (completing #(map + % %2)
               #(apply format "#%02X%02X%02X" (map (fn [c] (quot c (count rgbs))) %)))
   [0 0 0]
   rgbs))



(defn rgb84 [rgbs]
  (transduce
   (comp
    (mapcat #(map (fn [i] (subs % i (+ i 2))) [1 3 5]))
    (map #(Integer/parseInt % 16))
    (partition-all 3))
   (completing #(map + % %2)
               #(apply format "#%02X%02X%02X" (map (fn [c] (quot c (count rgbs))) %)))
   [0 0 0]
   rgbs))


;; very slow
(defn rgb91 [rgbs]
  (let [cnt (count rgbs)
        cs  (->> rgbs
                (mapcat #(map (fn [i] (subs % i (+ i 2))) [1 3 5]))
                (map #(Integer/parseInt % 16)))
        r (quot (reduce + (take-nth 3 cs)) cnt)
        g (quot (reduce + (take-nth 3 (next cs))) cnt)
        b (quot (reduce + (take-nth 3 (nnext cs))) cnt)]
    (format "#%02X%02X%02X" r g b)))



(defn reduce-nth [rf init coll]
  (let [initial (transient (vec init))
        cnt (count initial)
        ilast (dec cnt)]
    (if-not (pos-int? cnt)
      init
      (loop [result initial coll (seq coll) i 0]
        (if (seq coll)
          (recur (assoc! result i (rf (nth result i) (first coll)))
                 (rest coll)
                 (if (= i ilast) 0 (inc i)))
          (persistent! result))))))

;; better than 91 but still slow
(defn rgb92 [rgbs]
  (let [cnt (count rgbs)
        cs  (->> rgbs
                (mapcat #(map (fn [i] (subs % i (+ i 2))) [1 3 5]))
                (map #(Integer/parseInt % 16)))
        sums (reduce-nth + [0 0 0] cs)]
    (apply format "#%02X%02X%02X" (map #(quot % cnt) sums))))





     
#_ (require '[clojure.edn :as edn])

(defn read-rgb [rgbstr]
  (let [rgb (clojure.edn/read-string (str "0x" (subs rgbstr 1)))
        r (unsigned-bit-shift-right rgb 16)
        g (bit-and 0xFF (unsigned-bit-shift-right rgb 8))
        b (bit-and 0xFF rgb)]
    [r g b]))

(defn rgb-mix0 [rgbs]
  (let [cs (map read-rgb rgbs)
        cnt (count cs)
        avg-nth (fn [n] (long (/ (reduce #(+ % (nth %2 n)) 0.5 cs) cnt)))]
    (format "#%02X%02X%02X" (avg-nth 0) (avg-nth 1) (avg-nth 2))))










(defn hexify [r g b]
  (format "#%02X%02X%02X" r g b))

(defn avg [coll]
  (long (/ (reduce + 0.5 coll) (count coll))))


(defn rgb-mix1 [rgbs]
  (->> rgbs
       (map read-rgb)
       (apply sequence (map list))
       (map avg)
       (apply hexify)))




(defn third [coll]
  (first (nnext coll)))

;; slightly faster
(defn rgb-mix2 [rgbs]
  (let [cs (map read-rgb rgbs)]
    (format "#%02X%02X%02X"
            (avg (map first cs))
            (avg (map second cs))
            (avg (map third cs)))))




(defn avg-nth [n cs]
  (long (/ (reduce #(+ % (nth %2 n)) 0.5 cs) (count cs))))


(defn rgb-mix5 [rgbs]
  (let [cs (map read-rgb rgbs)]
    (format "#%02X%02X%02X"
            (avg-nth 0 cs)
            (avg-nth 1 cs)
            (avg-nth 2 cs))))



(defn avg-cnt [n cs cnt]
  (long (/ (reduce #(+ % (nth %2 n)) 0.5 cs) cnt)))

;; faster
(defn rgb-mix6 [rgbs]
  (let [cs (map read-rgb rgbs)
        cnt (count cs)]
    (format "#%02X%02X%02X"
            (avg-cnt 0 cs cnt)
            (avg-cnt 1 cs cnt)
            (avg-cnt 2 cs cnt))))













(defn rgb-components [rgbstr]
  (let [rgb (edn/read-string (str "0x" (subs rgbstr 1)))]
    [(bit-and 0xFF0000 rgb)
     (bit-and 0xFF00 rgb)
     (bit-and 0xFF rgb)]))




(defn hexstr [r g b]
  (format "#%06X" (+ r g b)))

;; not faster
(defn rgb3 [rgbs]
  (->> rgbs
       (map rgb-components)
       (apply sequence (map list))
       (map avg)
       (map bit-and [0xFF0000 0xFF00 0xFF])
       (apply hexstr)))


(defn rgbs->long [rgbstr]
  (edn/read-string (str "0x" (subs rgbstr 1))))
 


(defn smoke-rgb
 ([] (smoke-rgb rgb-mix))
 ([mix]
  (assert (= (mix ["#FFFFFF" "#000000"]) "#7F7F7F"))
  ;; or "#808080" depending on how you round
  (assert (= (mix ["#FF0000" "#00FF00" "#0000FF"]) "#555555"))
  (assert (= (mix ["#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF"]) "#FFFFFF"))
  (assert (= (mix ["#FFFFF1" "#FFFFF1" "#FFFFF1" "#FFFFF1" "#FFFFF1" "#FFFFF1"]) "#FFFFF1"))
  (assert (= (mix ["#000000" "#000000" "#000000" "#000000" "#000000" "#000000"]) "#000000"))
  true))
