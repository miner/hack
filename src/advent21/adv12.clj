(ns advent21.adv12
  (:require [clojure.string :as str]))


;;; cave paths
;;; UPCASE are large, allowing multiple visits in a path
;;; lowcase are small and can be visited only once in a path

(def simple-conn-input
"start-A
start-b
A-c
A-b
b-d
A-end
b-end") 
  
(def sample-conn-input
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc" )

(def conn-input
"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")


(defn parse-conns [input]
  (reduce (fn [g [a b]]
            (-> g
                (update a conj b)
                (update b conj a)))
          {}
          (map #(str/split % #"[-]") (str/split-lines input))))

;; path might need set of lower nodes to cut off but we could search instead ???

(defn lower? [s]
  (let [c (long (first s))]
    (<= (long \a) c (long \z))))

(defn extend1 [g path node]
  (when-not (and (lower? node) (first (filter #(= % node) path)))
    (conj path node)))

(defn extend-path [g path]
  (keep #(extend1 g path %) (g (peek path))))

(defn search-paths [graph]
  (loop [paths [["start"]] solutions []]
    (if (empty? paths)
      solutions
      (let [sols (filter #(= (peek %) "end") paths)
            ps (remove #(= (peek %) "end") paths)]
        (recur (mapcat #(extend-path graph %) ps) (into solutions sols))))))

(defn answer1 [input]
  (count (search-paths (parse-conns input))))

    


;; for part 2, allow one small cave to have double visit, but neither start or end.
;; change state to map
;; {:path [start a b a end] :double t/f :nodes #{a b c}}

(defn extend1b [p node]
  (let [seen? ((:nodes p) node)]
    (cond (not seen?) (-> p (update :path conj node) (update :nodes conj node))
          (not (lower? node)) (-> p (update :path conj node) (update :nodes conj node))
          (and (not= node "start") (not (:double p)))
              (-> p (update :path conj node) (assoc :double true))
          :else nil)))

(defn extend-path2 [g p]
  (keep #(extend1b p %) (g (peek (:path p)))))

(defn search-paths2 [graph]
  (loop [ps [{:path ["start"] :nodes #{"start"} :double false}] solutions []]
    (if (empty? ps)
      solutions
      (let [sols (filter #(= (peek (:path %)) "end") ps)
            cps (remove #(= (peek (:path %)) "end") ps)]
        (recur (mapcat #(extend-path2 graph %) cps) (into solutions sols))))))

(defn answer2 [input]
  (count (search-paths2 (parse-conns input))))






  
(def real-input
"pf-pk
ZQ-iz
iz-NY
ZQ-end
pf-gx
pk-ZQ
ZQ-dc
NY-start
NY-pf
NY-gx
ag-ZQ
pf-start
start-gx
BN-ag
iz-pf
ag-FD
pk-NY
gx-pk
end-BN
ag-pf
iz-pk
pk-ag
iz-end
iz-BN")
