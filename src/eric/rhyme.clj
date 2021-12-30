(ns eric.rhyme)

;; https://gist.github.com/ericnormand/862cbf3d868238a83e78d27a19b50ff0

;;BUG what if first mapped then same!  Basically, have to check if val is already an
;;assigned key????  Question: is uniqueness of mapping required?  can A->B and C->B????
;;seems wrong


;; SAVE -- but still buggy! on same ref+other ref
(defn badbugequiv? [astr bstr]
  (when (= (count astr) (count bstr))
    (reduce (fn [m [a b]]
              (if-let [ma (m a)]
                (if (= ma b)
                  m
                  (reduced nil))
                (if (= a b)
                  m
                  (assoc m a b))))
            {}
            (map vector astr bstr))))

;; need ::same #{a b c} to mark same map
;; then dissoc ::same at end


(defn badeq? [astr bstr]
  (when (= (count astr) (count bstr))
    (-> (reduce (fn [m [a b]]
                  (let [ma (m a)
                        sa (contains? (::same m) a)
                        sb (contains? (::same m) b)
                        eq (= a b)]
                    (cond sa (if eq m (reduced nil))
                          sb (if eq m (reduced nil))
                          (= ma b) m
                          (and (nil? ma) eq) (update m ::same conj a)
                          (nil? ma) (assoc m a b)
                          :else (reduced nil))))
                {::same #{}}
                (map list astr bstr))
        (dissoc ::same))))


(defn bug-eq [astr bstr]
  (when (= (count astr) (count bstr))
    (reduce (fn [m [a b]]
              (if (= (m a b) b)
                (assoc m a b)
                (reduced nil)))
            {}
            (sequence (comp (map vector) (remove #(apply = %))) astr bstr))))


;; Ugly but works for edge cases
;; keeps track of reverse map (with LIST key) to guarantee uniqueness
;; but lots of cleanup so not pretty
(defn zeq [astr bstr]
  (when (= (count astr) (count bstr))
    (reduce-kv (fn [m k v]
                 (if (and (char? k) (not= k v))
                   (assoc m k v)
                   (or m {})))
               nil
               (reduce (fn [m [a b]]
                         (if (and (= (m a b) b) (= (m (list b) a) a))
                           (assoc m a b (list b) a)
                           (reduced nil)))
                       {}
                       (map list astr bstr)))))


;; works
(defn zeq2 [astr bstr]
  (when (= (count astr) (count bstr))
    (let [mp (reduce (fn [m [a b]]
                       (if (and (= (m a b) b) (= (m (list b) a) a))
                         (assoc m a b (list b) a)
                         (reduced nil)))
                     {}
                     (map list astr bstr))]
      (when mp
        (reduce-kv (fn [m k v]
                     (if (and (char? k) (not= k v))
                       (assoc m k v)
                       m))
                   {}
                   mp)))))
    

(defn bugzeq3 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (sort (vals (frequencies astr)))
                  (sort (vals (frequencies bstr)))))
      (reduce (fn [m [a b]]
                (if (= (m a b) b)
                  (assoc m a b)
                  (reduced nil)))
              {}
              (map list astr bstr)))))

(defn bugzeq4 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (sort (vals (frequencies astr)))
             (sort (vals (frequencies bstr))))
      (reduce (fn [m [a b]]
                (if (= (m a b) b)
                  (assoc m a b)
                  (reduced nil)))
              {}
              (map list astr bstr)))))


(defn bugzeq5 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (sort (vals (frequencies astr)))
             (sort (vals (frequencies bstr))))
      (reduce (fn [m [a b]]
                (cond (= (m a) b) m
                      (nil? (m a)) (assoc m a b)
                      :else (reduced nil)))
              {}
              (map list astr bstr)))))

;;; works
(defn zeq5 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (sort (vals (frequencies astr)))
             (sort (vals (frequencies bstr))))
      (reduce (fn [m [a b]]
                (cond (= (m a) b) m
                      (= a b) m
                      (nil? (m a)) (assoc m a b)
                      :else (reduced nil)))
              {}
              (map list astr bstr)))))

;; much faster
(defn feq [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [fa (frequencies astr)
            fb (frequencies bstr)]
        (reduce-kv (fn [m i a]
                     (let [b (nth bstr i)]
                       (cond (not= (fa a) (fb b)) (reduced nil)
                             (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc m a b)
                             :else (reduced nil))))
                   {}
                   (vec astr))))))

;; slightly slower
(defn feq2 [astr bstr]
  (cond (= astr bstr) {}
        (not= (count astr) (count bstr)) nil
        :else (let [fa (frequencies astr)
                    fb (frequencies bstr)]
                (when (= (map fa astr) (map fb bstr))
                  (reduce-kv (fn [m i a]
                               (let [b (nth bstr i)]
                       (cond (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc m a b)
                             :else (reduced nil))))
                   {}
                   (vec astr))))))

;; slower than feq2
(defn feq3 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [fa (frequencies astr)
            fb (frequencies bstr)]
        (when (= (map fa astr) (map fb bstr))
          (reduce (fn [m [a b]]
                    (cond (= (m a) b) m
                          (= a b) m
                          (nil? (m a)) (assoc m a b)
                          :else (reduced nil)))
                   {}
                   (map list astr bstr)))))))


(defn feq4 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [zab (zipmap astr bstr)
            zba (zipmap bstr astr)]
        (when (and (= (map zab astr) (seq bstr))
                   (= (map zba bstr) (seq astr)))
          (reduce-kv (fn [m a b]
                       (if (= a b)
                         (dissoc m a)
                         m))
                     zab
                     zab))))))






(defn smoke-rhyme [equiv?]
  (assert (= (equiv? "ABCB" "XALA")  {\A \X \B \A \C \L}))
  (assert (= (equiv? "A" "A")  {}))
  (assert (= (equiv? "A" "B")  {\A \B}))
  (assert (= (equiv? "ABB" "YXX") {\A \Y \B \X}))
  (assert (= (equiv? "ABBA" "CDDC") {\A \C \B \D}))
  (assert (= (equiv? "ABBA" "CBBC") {\A \C}))
  (assert (nil? (equiv? "AB" "A")))
  (assert (nil? (equiv? "ABB" "XXY")))
  (assert (nil? (equiv? "ABCA" "ABBA")))
  (assert (nil? (equiv? "ABBA" "ACBA")))
  true)

