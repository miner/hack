(ns miner.bag)

;;; https://www.juxt.pro/blog/clojure-ds/
;;; an example of an implementation for "bags" (or multi-sets) in Clojure.

(defn bag [] {:size 0 :content {}})

(defn add [bag ele]
  (-> bag
      (update :size inc)
      (update-in [:content ele] (fnil + 0) 1)))

(defn rmv [{content :content :as bag} ele]
  (let [cnt (get content ele)]
    (-> bag
        (update :size dec)
        (assoc :content (cond
                          (nil? cnt) (throw (ex-info "No such element" {:ele ele}))
                          (= cnt 1) (dissoc content ele)
                          :else (update content ele - 1))))))

;; contains?' and count' are quoted so that we are not overriding Clojure's core functions.

(defn contains?' [bag ele]
  (some? (get-in bag [:content ele])))

(defn count' [bag ele]
  (get-in bag [:content ele]))

(defn size [bag] (:size bag))


(comment
  
(-> (bag) (add "foo") (add "foo") (rmv "foo") (add "bar"))
;; => {:size 2, :content {"foo" 1, "bar" 1}}
(contains?' *1 "foo")
;; => true
(count' *2 "foo")
;; => 1

)


;;; a second approach.  More complicated but works with all the Clojure collection functions
;;; if you implement the right interfaces.

(deftype Bag [size content]
  clojure.lang.IPersistentSet

  (seq [this] (seq content))

  (count [this] size)

  (cons [this o]
    (Bag. (inc size) (update content o (fnil + 0) 1)))

  (empty [this] (Bag. 0 {}))

  (equiv [this other] (= this other))

  (disjoin [this key]
    (let [cnt (get content key)]
      (Bag. (dec size)
            (cond
              (nil? cnt) (throw (ex-info "No such key" {:key key}))
              (= cnt 1) (dissoc content key)
              :else (update content key - 1)))))

  (contains [this key] (some? (get content key)))

  (get [this key] (get content key)))

(defn bag2 [] (Bag. 0 {}))


;;; SEM:  I would consider using a regular map with a distinguised key for total count.  Or
;;; metadata for the caching the count.  If there's no meta, count up the results.

(defn bag3-count [bag]
  (or (::count bag) (reduce-kv (fn [sum _ v] (+ sum v)) 0 (dissoc bag ::count))))

(defn add3 [bag el]
  (-> bag
      (assoc ::count (inc (bag3-count bag)))
      (assoc el (inc (or (bag el) 0)))))

(defn rmv3 [bag el]
  (if-let [cel (bag el)]
    (let [bag (assoc bag ::count (dec (bag3-count bag)))]
      (if (= cel 1)
        (dissoc bag el)
        (assoc bag el (dec cel))))
    (throw (ex-info "No such element" {:missing-element el}))))
    
(defn bag3
  ([] {::count 0})
  ([& items] (reduce add3 (bag3) items)))
  
