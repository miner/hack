(ns advent23.day08
  (:require [clojure.string :as str]))

;;; Original code from blog:
;;; https://cestlaz.github.io/post/advent-2023-day-08/

(def sample "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")


(def sample2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

;; (u/load-data 2023 8)
(def data (slurp (clojure.java.io/resource "day08.dat")))

(defn parse-graphlines [graphlines]
  (reduce (fn [g next]
            (let [ [[_ k a b]] (re-seq #"(.{3}) = \((.{3}), (.{3})\)" next)
                  ]
              (assoc g k [a b])))
          {} graphlines)
                  
  )

(defn parse [data]
  (let [[dirs graphdata] (str/split data #"\n\n")
        graph (->> graphdata
                   str/split-lines
                   parse-graphlines
                   )
        ]
    [dirs graph]))

(def path (first (parse sample2)))
(def graph (second (parse sample2)))

(defn part1 [data]
  (let [ [path graph] (parse data) ]
    (loop [dirs (cycle path)
           current "AAA"
           count 0
           ]
      (if (=  current "ZZZ")
        count
        (let [move   (first dirs)
              [l r]  (graph current)
              next (if (= move \L) l r)]
          (recur (rest dirs) next  (inc count)))))
    ))  




;;; my solution, similar performance                    
(defn sem1 [data]
  (let [ [path graph] (parse data) ]
    (reduce (fn [[node depth] mv]
              (if (= node "ZZZ")
                (reduced depth)
                [(nth (get graph node) (if (= mv \L) 0 1)) (inc depth)]))
            ["AAA" 0]
            (cycle path))))




;; (part1 data)

(def p2sample "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(defn get-cycle-length [start path graph]
    (loop [dirs (cycle path)
           current start 
           count 0
           ]
      (if (str/ends-with? current "Z")
        count
        (let [move   (first dirs)
              [l r]  (graph current)
              next (if (= move \L) l r)]
          (recur (rest dirs) next  (inc count))))
    ))  

;;; SEM external utilities in original author's code

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (abs (* a b))
     (gcd a b)))

(require '[clojure.java.io :as io])
(require '[clj-http.client :as client]

(defn download-data
  "if file doesn't already exist, download the data and store in
  data/day#.dat"
  [year day]
  (let [fname (format "day%02d.dat" day)
        url (str "https://adventofcode.com/" year "/day/" day "/input")
        cookie "session=53616c7465645f5fd459afd5fd38064616059a49ef7f3073b9880ffc77754861018cb4be3fc007e9161a5347572d3192f2de781aae394be78902a9d9634d5685"
        outfile (format "data/%s" fname)
        ]
    (if (not (.exists (io/file outfile)))
      (let [data (:body (client/get url {:headers {:Cookie cookie}}))]
        (spit outfile data))
      "File already exists"
      )))
    
(defn load-data [year day]
  (download-data year day)
  (let [fname (format "day%02d.dat" day)]
  (slurp (str "data/" fname))))

;;; end of original util


(defn part2 [data]
  (let [[path graph] (parse data )
        starts (filter #(str/ends-with? % "A") (keys graph))
        cycle-lengths (map #(get-cycle-length % path graph) starts)
        ]
    (reduce lcm cycle-lengths)))

;; (part2 data)

