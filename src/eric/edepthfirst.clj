(ns miner.edepthfirst)


;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-387-to-stack-or-not-to-stack/

;; A node in the tree is a sequence. Each node has one value and zero or more children.
;; [value child1 child2 child3 ...]

;; Eric
(defn dfs [value node]
  (or (= value (first node))              ;; check current node
      (some #(dfs value %) (rest node)))) ;; recurse down each child


;; Eric
(defn dfs-stack [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (if (empty? branches)
      false ;; no more branches, so fail
      (let [node               (peek branches)
            remaining-branches (pop  branches)]
        (or (= value (first node))
            (recur (into remaining-branches (reverse (rest node)))))))))


(def xxx [:a [:b [:c] [:d [:e [:f] [:g]] [:h]] [:i]] [:j]])



(defn smoke-stack [dfs]
  (assert (empty? (remove #(dfs % xxx) [:a :b :c :d :e :f :g :h :i :j])))
  true)


;; SEM -- the `reverse` is a red flag
;; certainly not needed for simple search

(defn dfs2 [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (if (empty? branches)
      false ;; no more branches, so fail
      (let [node               (first branches)
            remaining-branches (rest  branches)]
        (or (= value (first node))
            (recur (concat (rest node) remaining-branches)))))))

(defn dfs3 [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (if (empty? branches)
      false ;; no more branches, so fail
      (let [node               (first branches)
            remaining-branches (rest  branches)]
        (or (= value (nth node 0))
            (recur (concat (subvec node 1) remaining-branches)))))))

;; slightly faster than other explict stacks, still slower than recursive dfs
(defn dfs4 [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (when (seq branches)
      (let [node               (first branches)
            remaining-branches (rest  branches)]
        (or (= value (nth node 0))
            (recur (concat (subvec node 1) remaining-branches)))))))


(defn dfs5 [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (when (seq branches)
      (let [node               (first branches)
            remaining-branches (rest  branches)]
        (or (= value (nth node 0))
            (recur (into remaining-branches (rseq (subvec node 1)))))))))

;; slow -- all vector
(defn dfs6 [value node]
  (loop [branches [node]]
    (when (pos? (count branches))
      (let [node               (nth branches 0)
            remaining-branches (subvec branches 1)]
        (or (= value (nth node 0))
            (recur (into remaining-branches (subvec node 1))))))))











;; change the representation to list and allow naked nodes
(def yyy '(:a (:b :c (:d (:e :f :g) :h) :i) :j))

(defn dfs7 [value node]
  (loop [branches (list node)] ;; lists act like stacks
    (when (seq branches)
      (let [node               (first branches)
            remaining-branches (rest  branches)]
        (if (sequential? node)
          (or (= value (first node))
              (recur (concat (rest node) remaining-branches)))
          (or (= value node)
              (recur remaining-branches)))))))



(defn smoke-nodes [dfs]
  (assert (empty? (remove #(dfs % yyy) [:a :b :c :d :e :f :g :h :i :j])))
  true)
