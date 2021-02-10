(ns miner.epal
  (:require [clojure.string :as str]))

;;;  See also my files pal.clj and palindrome.clj
;;; Those had ways to look for longest palidromic substring.  Now we're looking for minimum
;;; addition to make a palindrome (but not necessarily a real word).

;;; Challenge: Your task is to write a function that adds a minimum of letters to the end of
;;; a string to make it a palindrome.

;; https://gist.github.com/ericnormand/bf1178a04c7b28cdaf26e0fb3257defe



;;; better answer at end of file

(defn ->palindrome3 [s]
  (let [cnt (count s)
        pal? (fn [s] (when (= s (str/reverse s)) s))]
    (some pal? (map #(str s (subs (str/reverse s) % cnt)) (range cnt 0 -1)))))

;; a bit faster
(defn ->palindrome2 [s]
  (let [cnt (count s)
        r (str/reverse s)
        pal (fn [s] (when (= s (str/reverse s)) s))]
    (first (sequence (comp (map #(str s (subs r % cnt))) (filter pal) (take 1))
                     (range cnt 0 -1)))))



(defn smoke-pal
  ([] (smoke-pal ->palindrome))
  ([->palindrome]
   (assert (= (->palindrome "race") "racecar"))
   (assert (= (->palindrome "mad")  "madam"))
   (assert (= (->palindrome "mirror") "mirrorrim"))
   (assert (= (->palindrome "madam") "madam"))
   true))





(defn palindrome? [s]
  (= s (str/reverse s)))

(defn ->palindrome1 [s]
  (let [cnt (count s)
        r (str/reverse s)]
    (first (filter palindrome? (map #(str s (subs r % cnt)) (range cnt 0 -1))))))






;;; Idea: shortest addition is basically longest pal of reverse, plus the extras added on again




;;; Fastest implementation is to use interop to access chars in string.  Don't create new
;;; strings.  Don't need to vectorize.  Just .charAt.


;; use indexing to look at char in existing s without realizing substrings

(defn ->plx [s]
  (let [cnt (count s)
        palext? (fn [ext]
                  (let [extended (dec (+ cnt ext))]
                    (loop [front 0 back extended]
                      (or (>= front back)
                          (and (= (.charAt ^String s front)
                                  (.charAt ^String s (if (>= back cnt) (- extended back) back)))
                               (recur (inc front) (dec back)))))))]
    (first (for [ext (range cnt)
                 :when (palext? ext)]
             (str s (subs (str/reverse s) (- cnt ext) cnt))))))



;; faster
(defn ->plx2 [s]
  (let [cnt (count s)
        palext? (fn [ext]
                  (let [extended (dec (+ cnt ext))]
                    (loop [front 0 back extended]
                      (or (>= front back)
                          (and (= (.charAt ^String s front)
                                  (.charAt ^String s (if (>= back cnt) (- extended back) back)))
                               (recur (inc front) (dec back)))))))
        ext (first (filter palext? (range cnt)))]
    (str s (subs (str/reverse s) (- cnt ext) cnt))))

;; not faster??
(defn ->plx3 [s]
  (let [cnt (count s)
        palext? (fn [ext]
                  (let [extended (dec (+ cnt ext))]
                    (loop [front 0 back extended]
                      (or (>= front back)
                          (and (= (.charAt ^String s front)
                                  (.charAt ^String s (if (>= back cnt) (- extended back) back)))
                               (recur (inc front) (dec back)))))))
        ext (first (filter palext? (range cnt)))]
    (str s (str/reverse (subs s 0 ext)))))


;; faster
(defn ->plx4 [s]
  (let [cnt (count s)
        palext? (fn [ext]
                  (let [extended (dec (+ cnt ext))]
                    (loop [front 0 back extended]
                      (or (>= front back)
                          (and (= (.charAt ^CharSequence s front)
                                  (.charAt ^CharSequence s (if (>= back cnt)
                                                             (- extended back) back)))
                               (recur (inc front) (dec back)))))))
        ext (first (filter palext? (range cnt)))]
    (str (subs s 0 ext) (str/reverse s))))


;; fastest but getting ugly
(defn ->plx6 [^CharSequence s]
  (let [cnt (.length s)
        palext? (fn [ext]
                  (let [extended (dec (+ cnt ext))]
                    (loop [front 0 back extended]
                      (or (>= front back)
                          (and (= (.charAt s front)
                                  (.charAt s (if (>= back cnt) (- extended back) back)))
                               (recur (inc front) (dec back)))))))
        ext (first (filter palext? (range cnt)))]
    (-> (StringBuilder. s)
        .reverse
        (.insert (int 0) s (int 0) (int ext))
        .toString)))



;; steffan-westcott -- cleaver insight on starts-with? vs. reverse
;; but reductions is a bit slower than my way below
(defn sw->palindrome [s]
  (->> (reductions str "" s)
    (map #(str % (str/reverse s)))
    (filter #(str/starts-with? % s))
    first))

;; credit to steffan-westcott -- cleaver insight on starts-with? vs. reverse
;; SEM hacked for transducers, pretty good
(defn ->palindrome [s]
  (let [r (str/reverse s)]
    (first (sequence (comp (map #(str (subs s 0 %) r))
                           (filter #(str/starts-with? % s))
                           (take 1))
                     (range (count s))))))

;; might be generally useful pattern
(defn xsome [xform pred coll]
  (first (sequence (comp xform (filter pred) (take 1)) coll)))

;; surprisingly a bit faster with xsome
(defn palin [s]
  (let [r (str/reverse s)]
    (xsome (map #(str (subs s 0 %) r))
           #(str/starts-with? % s)
           (range (count s)))))

