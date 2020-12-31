(ns miner.econsec
  (:require [clojure.string :as str]))


;;; https://gist.github.com/ericnormand/3a39e9093692b1ee3dbbb612d739e468
;;;
;;; Eric Normand's Clojure Challenge
;;
;; Write a function that takes a string of digits. Try to break up the digits into
;; consecutive integers. If you can, return them, otherwise, return nil.



;; Note, it's required to have more than one consecutive integer in the string, otherwise
;; fail with a nil result.  I choose not to throw for that case.


#_
(require '[clojure.string :as str])

;; revised submission
(defn consec [nstr]
  (let [parse-inc (fn [nstr result]
                    (let [target (inc (peek result))
                          tstr (str target)]
                      (when (str/starts-with? nstr tstr)
                        (if (= nstr tstr)
                          (conj result target)
                          (recur (subs nstr (count tstr)) (conj result target))))))]
    (if (str/starts-with? nstr "0")
      (parse-inc (subs nstr 1) [0])
      (first (keep (fn [end]
                     (parse-inc (subs nstr end) [(Long/parseLong (subs nstr 0 end))]))
                   (range 1  (inc (quot (count nstr) 2))))))))




;;; BUG:  "0123456789"  leading zero will parse wrong  0123124
;;; leading 0 is mostly ignored but causes offset problems.
;;; Arguably, it should be considered single digit 0

;; original submission is buggy with leading "0"
(defn consec0 [nstr]
  (let [parse-inc (fn [nstr result]
                    (let [target (inc (peek result))
                          tstr (str target)]
                      (cond (= nstr tstr) (conj result target)
                            (str/starts-with? nstr tstr)
                                (recur (subs nstr (count tstr)) (conj result target))
                            :else nil)))]
    (first (keep (fn [end]
                   (parse-inc (subs nstr end) [(Long/parseLong (subs nstr 0 end))]))
                 (range (quot (count nstr) 2) 0 -1)))))


(defn smoke-consec
  ([] (smoke-consec consec))
  ([consec]
   (assert (= (consec "121314") [12 13 14]))
   (assert (nil? (consec "121315") ))
   (assert (= (consec "444445") [444 445]))
   (assert (= (consec "12") [1 2]))
   (assert (= (consec "9991000") [999 1000]))
   (assert (nil? (consec "1")))
   ;; NOT throws error on single 1
   (assert (= (consec "12345") [1 2 3 4 5]))
   (assert (= (consec "9999899999100000100001") [99998 99999 100000 100001]))
   ;; could trip on read-string reading octal
   (assert (nil? (consec "010910")))
   ;; leading zero could cause problems
   (assert (= (consec "0123") [0 1 2 3]))
   (assert (nil? (consec "01213")))
   true
   ))




;;; pretty good, but a little noisy with the `for` options -- keep is cleaner
;;; also buggy with leading "0"
(defn consec6 [nstr]
  (let [parse-inc (fn [nstr result]
                    (let [target (inc (peek result))
                          tstr (str target)]
                      (cond (= nstr tstr) (conj result target)
                            (str/starts-with? nstr tstr)
                                (recur (subs nstr (count tstr)) (conj result target))
                            :else nil)))]
    (first (for [end (range (quot (count nstr) 2) 0 -1)
                 :let [n (Long/parseLong (subs nstr 0 end))
                       res (parse-inc (subs nstr end) [n])]
                 :when res]
             res))))






;;; First try had too much searching.  10x time.  Also, buggy with leading "0" input.

(defonce long0 (long \0))

(defn chnum [ch]
  (- (long ch) long0))

;; note: allow overflow to bignums if necessary
(defn sum-digits [ds]
  (reduce (fn [s d] (+' (*' 10 s) d)) 0 ds))


(defn nconsec-WORKS [result target dv]
  (let [cnt (count dv)]
    ;;(println "dv" dv "targ" target)
    (if (zero? cnt)
      (when (> (count result) 1) result)
      (loop [end 1]
        (when (<= end cnt)
          (let [n (sum-digits (subvec dv 0 end))]
            (cond (or (nil? target) (= n target))
                      (if-let [res (nconsec-WORKS (conj result n) (inc n) (subvec dv end))]
                        res
                        (recur (inc end)))
                  (< n target) (recur (inc end))
                  :else nil)))))))


(defn nconsec1 [result target dv]
  (if (zero? (count dv))
    (when (> (count result) 1) result)
    (->> (for [end (range 1 (inc (count dv)))
               :let [n (sum-digits (subvec dv 0 end))]
               :while (or (nil? target) (<= n target))
               :when (or (nil? target) (= n target))]
           (when-let [sol (nconsec1 (conj result n) (inc n) (subvec dv end))]
             sol))
         (remove nil?)
         first)))

(defn consec1 [nstr]
  (nconsec1 [] nil (mapv chnum nstr)))





;; steffan-westcott -- short and correct, but much slower than mine
(defn consec-sw [s]
  (first (for [first-len (range 1 (-> s count (quot 2) inc))
               :let [first-num (read-string (subs s 0 first-len))]
               nums (iterate #(conj % (inc (peek %))) [first-num])
               :let [s' (apply str nums)]
               :while (<= (count s') (count s))
               :when (= s s')]
            nums)))

