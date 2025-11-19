(ns miner.states
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def states-string
"Alabama
Alaska
Arizona
Arkansas
California
Colorado
Connecticut
Delaware
Florida
Georgia
Hawaii
Idaho
Illinois
Indiana
Iowa
Kansas
Kentucky
Louisiana
Maine
Maryland
Massachusetts
Michigan
Minnesota
Mississippi
Missouri
Montana
Nebraska
Nevada
New Hampshire
New Jersey
New Mexico
New York
North Carolina
North Dakota
Ohio
Oklahoma
Oregon
Pennsylvania
Rhode Island
South Carolina
South Dakota
Tennessee
Texas
Utah
Vermont
Virginia
Washington
West Virginia
Wisconsin
Wyoming")

(def states (str/split-lines states-string))

(defn unused-chars1 []
  (remove (into #{} (comp (map str/upper-case) (mapcat seq)) states)
          (seq "ABCEDFGHIJKLMNOPQRSTUVWXYZ")))

(defn unused-chars2 []
  (remove (set (str/upper-case states-string)) "ABCEDFGHIJKLMNOPQRSTUVWXYZ"))


(defn unused-chars3 []
  (set/difference (set "ABCEDFGHIJKLMNOPQRSTUVWXYZ")
                  (set (str/upper-case states-string))))

;;; fastest
(defn unused-chars4 []
  (reduce disj (set "ABCEDFGHIJKLMNOPQRSTUVWXYZ") (str/upper-case states-string)))

          
(defn hex [n]
  (Long/toHexString n))

;;; borrowed from my bitset.clj bseq
(defn zbseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/highestOneBit n)]
        (recur (bit-xor n h) (conj bs (Long/numberOfTrailingZeros h)))))))



;;; bit twiddling
;;; just use low 5 bits for chars as index into long
(defn unused-chars-bits []
  (reduce (fn [r ch] (bit-clear r (bit-and 0x1F (int ch))))
          (dec (bit-set 0 27))
          (str/upper-case states-string)))

(defn unused-chars5 []
  (map #(char (+ (dec (long \A)) %)) (zbseq (unused-chars-bits))))


;;; inline bseq and unused-chars-bits, about the same speed so maybe not worth it
(defn unused-chars6 []
  (loop [n (reduce (fn [r ch] (bit-clear r (bit-and 0x1F (int ch))))
                   0x7FFFFFF ;(dec (bit-set 0 27)) 
                   (str/upper-case states-string))
         chs ()]
    (if (zero? n)
      chs
      (let [h (Long/highestOneBit n)]
        (recur (bit-xor n h)
               (conj chs (char (+ (dec (long \A)) (Long/numberOfTrailingZeros h)))))))))
