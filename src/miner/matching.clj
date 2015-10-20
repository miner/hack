(ns miner.matching
  (:require [clojure.core.match :as m])
  (:require [clojure.set :as s]))


(defmacro SEM-match-let [bindings & body]
  (let [bindvars# (take-nth 2 bindings)]
    `(let ~bindings
       (m/match [~@bindvars#]
                ~@body))))

(comment

  (match-let [x 1 y 2]
             [a (b :when #(= % x))] [:same a]
             :else :diff)

  )

(defn rewrite-equal-bindvars [bindv]
  (let [cnt (count bindv)
        dist (distinct bindv)]
    (if (== cnt (count dist))
      bindv
      ;; else mangle
      )))
      


(comment

  (match-let [x 1 y 3]
         [1 (or 3 4)] :1+34
         [_ b :when even?] :even
         [_ c :is (inc x)] :inc
         inc :inc)
  

  )

;; single symbol var is an iterative fn -- match succeeding values
;; `(vec (take ~(count bvars) (iterate ~f (first bvars))))
;;   [2 3 4]   matches inc
;;   [true false true]   matches not

;; maybe also a map-indexed fn notation

;; :every even?


(defn pattern-keyword? [kw]
  (#{:when :as} kw))


;; not supported for flattened syntax
(defn one-arg-pattern-keyword? [kw]
  (#{:seq ::vector} kw))


;; reduce-pat is twice as fast as the others
(defn reduce-pat [pat]
  (let [sentinel (Object.)
        [pp qq rr] (reduce (fn [[p q result] r]
                             (cond (= sentinel p) [q r result]
                                   (pattern-keyword? q) [sentinel sentinel (conj result (list p q r))]
                                   :else [q r (conj result p)]))
                           [sentinel sentinel []] pat)]
    (cond (and (= sentinel pp) (= sentinel qq)) rr
          (= sentinel pp) (conj rr qq)
          (= sentinel qq) (conj rr pp)
          :else (conj rr pp qq))))


(defn reduce-pat-works [pat]
  (let [sentinel (Object.)
        [pp qq rr] (reduce (fn [[p q result] r]
                             (cond (= sentinel p) [q r result]
                                   (pattern-keyword? q) [sentinel sentinel (conj result (list p q r))]
                                   :else [q r (conj result p)]))
                           [sentinel sentinel []] pat)]
    (cond (and (= sentinel pp) (= sentinel qq)) rr
          (= sentinel pp) (conj rr qq)
          (= sentinel qq) (conj rr pp)
          :else (conj rr pp qq))))



(defn bad-grp [pattern]
  (let [void (Object.)
        ;; void is a unique placeholder for nothing -- we can't use nil because that's a legal symbol in a pattern row
        [pp qq rr] (reduce (fn [[p q result] r]
                             (cond (= void p) [q r result]
                                   (pattern-keyword? q) [void void (conj result (list p q r))]
                                  ;; (vector? r) [q (group-top-level-keywords r) (conj result p)]
                                   ;; (and (list? r) (vector? (first r))) [q (cons (group-top-level-keywords (first r)) (rest r)) (conj result p)]
                                   :else [q r (conj result p)]))
                           [void void []] (conj pattern void void))]
    rr))




(defn pattern-keyword? [kw]
  (#{:when :as} kw))

(comment
(let [void (Object.)]
  ;; void is a unique placeholder for nothing -- we can't use nil because that's a legal symbol in a pattern row
(defn- group-top-level-keywords 
  "Returns a pattern with pattern-keywords (:when and :as) properly grouped.  The original pattern
may use the 'flattened' syntax at the top level.  For example, a 'flattened' pattern row like
[a b :when even?] is grouped as [a (b :when even?)]."
[pattern]
(if (vector? pattern)
  (let [[rr pp qq] (reduce (fn [[result p q] r]
                             (cond (= void p) [result q r]
                                   (pattern-keyword? q) [(conj result (list p q r)) void void]
                                   :else [(conj result p) q r]))
                           [[] void void] pattern)]
    (cond (and (= void pp) (= void qq)) rr
          (= void pp) (conj rr qq)
          (= void qq) (conj rr pp)
          :else (conj rr pp qq)))
  pattern)))
)

(defn grp
  "Returns a pattern with pattern-keywords (:when and :as) properly grouped.  The original pattern
may use the 'flattened' syntax at the top level.  For example, a 'flattened' pattern row like
[a b :when even?] is grouped as [a (b :when even?)]."
[pattern]
(if (vector? pattern)
  (let [void (gensym)]
    (first (reduce (fn [[result p q] r]
                     (cond (= void p) [result q r]
                           (and (not= void r) (pattern-keyword? q)) [(conj result (list p q r)) void void]
                           :else [(conj result p) q r]))
                   [[] void void]
                   (conj pattern void void))))
  pattern))


(defn literal-pattern? [pat]
  (or (keyword? pat) (number? pat) (nil? pat) (true? pat) (false? pat) (string? pat)))


(defn rgrp [pattern]
  (cond (vector? pattern)
        (let [void (gensym)]
          (first (reduce (fn [[result p q] r]
                           (cond (= void p) [result q r]
                                 (and (not= void r) (pattern-keyword? q)) [(conj result (list (rgrp p) q r)) void void]
                                 :else [(conj result (rgrp p)) q r]))
                         [[] void void]
                         (conj pattern void void))))
        (seq? pattern) (case (second pattern)
                          (:as |) (interpose (second pattern) (map rgrp (take-nth 2 pattern)))
                          (cons (rgrp (first pattern)) (rest pattern)))
        :else pattern))





(defn collect-wildcards [pat]
  (cond (= pat '_) nil
        (= pat '&) nil
        (symbol? pat) (list pat)
        (vector? pat) (mapcat collect-wildcards pat)
        (map? pat) (mapcat collect-wildcards (vals pat))
        (seq? pat) (case (second pat)
                      (:as |) (mapcat collect-wildcards (take-nth 2 pat))
                      (collect-wildcards (first pat)))
        :else nil))

;; somewhat faster that above
(defn find-wc [remaining result]
  (if-let [pats (seq remaining)]
    (let [pat (first pats)
          pats (rest pats)]
      (cond (or (= pat '_) (= pat '&)) (recur pats result)
            (symbol? pat) (recur pats (conj result pat))
            (vector? pat) (recur (concat pats pat) result)
            (map? pat) (recur (concat pats (vals pat)) result)
            (seq? pat) (case (second pat)
                         (:as |) (recur (concat pats (take-nth 2 pat)) result)
                         (recur (conj pats (first pat)) result))
            :else (recur pats result)))
    result))

;; was working, but too restrictive for OR
(defn- find-duplicate-wildcards [pattern]
  (loop [remaining pattern seen #{} dups #{}]
    (if-let [pattern (seq remaining)]
      (let [pat (first pattern)
            pats (rest pattern)]
        (cond (or (= pat '_) (= pat '&)) (recur pats seen dups)
              (symbol? pat) (if (contains? seen pat)
                              (recur pats seen (conj dups pat))
                              (recur pats (conj seen pat) dups))
              (vector? pat) (recur (concat pats pat) seen dups)
              (map? pat) (recur (concat pats (vals pat)) seen dups)
              (seq? pat) (case (second pat)
                           (:as |) (recur (concat pats (take-nth 2 pat)) seen dups)
                           (recur (conj pats (first pat)) seen dups))
              :else (recur pats seen dups)))
      dups)))



(defn wildcards-and-duplicates
  "Returns a vector of two elements: the set of all wildcards and the set of duplicate wildcards.  The underbar _ is excluded from both."
  [patterns]
  (loop [remaining patterns seen #{} dups #{}]
    (if-let [patterns (seq remaining)]
      (let [pat (first patterns)
            pats (rest patterns)]
        (cond (or (= pat '_) (= pat '&)) (recur pats seen dups)
              (symbol? pat) (if (contains? seen pat)
                              (recur pats seen (conj dups pat))
                              (recur pats (conj seen pat) dups))
              (vector? pat) (recur (concat pats pat) seen dups)
              (map? pat) (recur (concat pats (vals pat)) seen dups)
              (seq? pat) (case (second pat)
                           :as (recur (concat pats (take-nth 2 pat)) seen dups)
                           | (let [wds (map wildcards-and-duplicates (map list (take-nth 2 pat)))
                                   mseen (apply s/union (map first wds))]
                               (recur pats (s/union seen mseen) (apply s/union dups (s/intersection seen mseen) (map second wds))))
                           (recur (conj pats (first pat)) seen dups))
              :else (recur pats seen dups)))
      [seen dups])))

(defn- find-duplicate-wildcards [pattern]
  (second (wildcards-and-duplicates pattern)))


;; [([a 1] | [a 2]]

(def bbb '[([:black [:red [:red a x b] y c] z d] |
              [:black [:red a x [:red b y c]] z d] |
                 [:black a x [:red [:red b y c] z d]] |
                   [:black a x [:red b y [:red c z d]]])])

(def ccc '[x ([:black [:red [:red a x b] y c] z d] |
              [:black [:red a x [:red b y c]] z d] |
                [:black a x [:red [:red b y c] z d]] |
                  [:black aa x [:red [:black aa y c] z d]] |
                   [:black a x [:red b y [:red c z d]]])])

(def ddd '[a (b :when even?) ([c a & fs] :as g) _ nil true 10 (b | i | j | nil | k) m])
(def eee '[a (b :when even?) ([c a2 & fs] :as g) _ nil true 10 (b2 | i | j | nil | k) m])
(def fff (vector (list eee '| eee)))

;; but the problem is that it's too restrictive for OR patterns
(defn dup-wc-global-works [pattern]
  (loop [remaining pattern seen #{} dups ()]
    (if-let [pattern (seq remaining)]
      (let [pat (first pattern)
            pats (rest pattern)]
        (cond (or (= pat '_) (= pat '&)) (recur pats seen dups)
              (symbol? pat) (if (contains? seen pat)
                              (recur pats seen (conj dups pat))
                              (recur pats (conj seen pat) dups))
              (vector? pat) (recur (concat pats pat) seen dups)
              (map? pat) (recur (concat pats (vals pat)) seen dups)
              (seq? pat) (case (second pat)
                           :as (recur (concat pats (take-nth 2 pat)) seen dups)
                           | (recur (concat pats (take-nth 2 pat)) seen dups)
                           (recur (conj pats (first pat)) seen dups))
              :else (recur pats seen dups)))
      dups)))

;; destructing is slower
(defn dup-wc-ds [pattern]
  (loop [remaining pattern seen #{} dups ()]
    (if-let [ps (seq remaining)]
      (let [[pat & pats] ps]
        (cond (or (= pat '_) (= pat '&)) (recur pats seen dups)
              (symbol? pat) (if (contains? seen pat)
                              (recur pats seen (conj dups pat))
                              (recur pats (conj seen pat) dups))
              (vector? pat) (recur (concat pats pat) seen dups)
              (map? pat) (recur (concat pats (vals pat)) seen dups)
              (seq? pat) (case (second pat)
                           (:as |) (recur (concat pats (take-nth 2 pat)) seen dups)
                           (recur (conj pats (first pat)) seen dups))
              :else (recur pats seen dups)))
      dups)))

;; gets too much junk from guards
(defn bad-flatten-wildcards [pat]
  (filter (fn [p] (and (symbol? p) (not= p '_) (not= p '&))) (flatten pat)))


;; slower, probably because of redundant type checks
(defn collect-wc [pat]
  (filter (fn [p] (and (symbol? p) (not= p '_) (not= p '&)))
          (tree-seq sequential?
                    (fn children [p] (cond (vector? p) p
                                           (map? p) (vals p)
                                           (seq? p) (case (second p)
                                                      (:as |) (take-nth 2 p)
                                                      (list (first p)))))
                    pat)))


(defn find-duplicate-wildcards [pat]
  (keep (fn [[sym cnt]] (when (> cnt 1) sym)) (frequencies (collect-wildcards pat))))

(defn find-dup-wc [pat]
  (keep (fn [[sym cnt]] (when (> cnt 1) sym)) (frequencies (find-wc pat ()))))
