(ns eric.rhyme)

;; https://gist.github.com/ericnormand/fe78cac5bd3e9aced964234c04c711c4

;; Question: is uniqueness of mapping required?  Can A->B and C->B????
;; My opinion is that mapping should be "bijective".  Others agreed and revised accordingly
;; on the gist.



;;; definitely worth special case for =
;;; based on @sw but faster sig, and faster cleanup A->A

(defn equiv? [s1 s2]
  (let [signature (fn [s] (mapv #(clojure.string/index-of s %) s))]
    (if (= s1 s2)
      {}
      (when (= (signature s1) (signature s2))
        (let [z (zipmap s1 s2)]
          (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                     z
                     z))))))







(defn equiv7? [s1 s2]
  (let [signature (fn [s] (mapv #(.lastIndexOf ^String s (int %)) s))]
    (if (= s1 s2)
      {}
      (when (= (signature s1) (signature s2))
        (let [zzz (zipmap s1 s2)]
          (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                     zzz
                     zzz))))))

(defn equiv8? [s1 s2]
  (if (= s1 s2)
    {}
    (let [signature (fn [s]
                      (let [tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                       (transient {})
                                       (range (count s)))]
                        (mapv tm s)))]
      (when (= (signature s1) (signature s2))
        (let [zzz (zipmap s1 s2)]
          (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                     zzz
                     zzz))))))



(defn equiv9? [s1 s2]
  (let [signature (fn [s] (let [tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                           (transient {})
                                           (range (count s)))]
                            (mapv tm s)))]
    (if (= s1 s2)
      {}
      (when (= (signature s1) (signature s2))
        (let [zzz (zipmap s1 s2)]
          (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                     zzz
                     zzz))))))



;;; BUG on freq comparison -- easy to spoof same counts but diff order
;;; correct way is something like @sw (mapv (zipmap s (range)) s)

(defn signa [s]
  (let [m (into {} (map-indexed (fn [i c] [c i])) s)]
    (mapv m s)))

(defn signa2 [s]
  (mapv (zipmap s (range)) s))

(defn signa3 [s]
  (let [tm (reduce (fn [m i] (assoc! m (nth s i) i)) (transient {}) (range (count s)))]
    (mapv tm s)))



(defn signa4 [s]
  (mapv #(.lastIndexOf ^String s (int %)) s))


(defn veq22 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (signa3 astr) (signa3 bstr)))
      (let [zab (zipmap astr bstr)
            tres (reduce (fn [m i]
                           (let [a (nth astr i)
                                 b (nth bstr i)]
                             (cond (not= (zab a) b) (reduced nil)
                                   (= (m a) b) m
                                   (= a b) m
                                   (nil? (m a)) (assoc! m a b)
                                   :else (reduced nil))))
                         (transient {})
                         (range (count astr)))]
        (when tres
          (persistent! tres))))))


(defn veq24 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (signa4 astr) (signa4 bstr)))
      (let [zab (zipmap astr bstr)
            tres (reduce (fn [m i]
                           (let [a (nth astr i)
                                 b (nth bstr i)]
                             (cond (not= (zab a) b) (reduced nil)
                                   (= (m a) b) m
                                   (= a b) m
                                   (nil? (m a)) (assoc! m a b)
                                   :else (reduced nil))))
                         (transient {})
                         (range (count astr)))]
        (when tres
          (persistent! tres))))))



(defn veq2 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (count (set astr)) (count (set bstr))))
      (let [zab (zipmap astr bstr)]
        (reduce (fn [m i]
                  (let [a (nth astr i)
                        b (nth bstr i)]
                       (cond (not= (zab a) b) (reduced nil)
                             (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc m a b)
                             :else (reduced nil))))
                   {}
                   (range (count astr)))))))

(defn veq21 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (count (set astr)) (count (set bstr))))
      (let [zab (zipmap astr bstr)
            tres (reduce (fn [m i]
                           (let [a (nth astr i)
                                 b (nth bstr i)]
                             (cond (not= (zab a) b) (reduced nil)
                                   (= (m a) b) m
                                   (= a b) m
                                   (nil? (m a)) (assoc! m a b)
                                   :else (reduced nil))))
                         (transient {})
                         (range (count astr)))]
        (when tres
          (persistent! tres))))))


;; OK but slowish
(defn veq [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (count (set astr)) (count (set bstr))))
      (let [va (vec astr)
            zab (zipmap va bstr)]
        (reduce-kv (fn [m i a]
                     (let [b (nth bstr i)]
                       (cond (not= (zab a) b) (reduced nil)
                             (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc m a b)
                             :else (reduced nil))))
                   {}
                   va)))))



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
    
;;; works, reasonably fast (2x teq)
;;; worried about freq counterfeit
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
  (assert (= (equiv? "ABBAC" "CBBCD") {\A \C \C \D}))
  (assert (nil? (equiv? "ABBAC" "CBBCC")))
  (assert (nil? (equiv? "AB" "A")))
  (assert (nil? (equiv? "ABB" "XXY")))
  (assert (nil? (equiv? "ABCA" "ABBA")))
  (assert (nil? (equiv? "ABBA" "ACBA")))
  (assert (nil? (equiv? "ABABAB" "AABBAB")))
  true)



;; fails for me
(defn sw1-equiv? [s1 s2]
  (let [m (zipmap s1 s2)]
    (when (= (map m s1) (seq s2))
      (into {} (remove #(apply = %)) m))))

;; corrected after my example
(defn sw-equiv? [s1 s2]
  (let [signature (fn [s] (map (zipmap (distinct s) (range)) s))]
    (when (= (signature s1) (signature s2))
      (into {} (remove #(apply = %)) (zipmap s1 s2)))))


;; based on sw, but faster
(defn sem-equiv? [s1 s2]
  (let [signature (fn [s] (mapv (zipmap s (range)) s))]
    (when (= (signature s1) (signature s2))
      (into {} (remove #(= (key %) (val %))) (zipmap s1 s2)))))

;; revised correct and faster
(defn sem2-equiv? [s1 s2]
  (let [signature (fn [s]
                    (let [tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                     (transient {})
                                     (range (count s)))]
                      (mapv tm s)))]
    (when (= (signature s1) (signature s2))
      (let [zzz (zipmap s1 s2)]
        (persistent!
         (reduce-kv (fn [m k v] (if (= k v) (dissoc! m k) m))
                    (transient zzz)
                    zzz))))))

;; faster without transients! I guess it hardly ever dissocs.
(defn sem3-equiv? [s1 s2]
  (let [signature (fn [s]
                    (let [tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                     (transient {})
                                     (range (count s)))]
                      (mapv tm s)))]
    (when (= (signature s1) (signature s2))
      (let [zzz (zipmap s1 s2)]
         (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                    zzz
                    zzz)))))


;;; definitely worth special case for =
(defn sem31-equiv? [s1 s2]
  (if (= s1 s2)
    {}
    (let [signature (fn [s]
                      (let [tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                       (transient {})
                                       (range (count s)))]
                        (mapv tm s)))]
      (when (= (signature s1) (signature s2))
        (let [zzz (zipmap s1 s2)]
          (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                     zzz
                     zzz))))))



;; not faster sig
(defn sem4-equiv? [s1 s2]
  (let [signature (fn [s]
                    (let [cnt (count s)
                          tm (reduce (fn [m i] (assoc! m (nth s i) i))
                                     (transient {})
                                     (range cnt))]
                      (reduce (fn [r i] (+ (tm (nth s i)) (* cnt r))) 0 (range cnt))))]
    (when (= (signature s1) (signature s2))
      (let [zzz (zipmap s1 s2)]
         (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                    zzz
                    zzz)))))



(defn zeq7 [a b]
  (let [zab (zipmap a b)
        zba (zipmap b a)]
    (when (and (= (mapv zab a) (seq b))
               (= (mapv zba b) (seq a)))
      (reduce-kv (fn [m k v] (if (= k v) (dissoc m k) m))
                 zab
                 zab))))

;;      (into {} (remove #(= (key %) (val %))) (zipmap s1 s2)))))

;; works but slow
(defn nr-equiv? [a b]
  (let [index #(into {} (map vector (distinct %) (range)))]
    (when (= (map (index a) a) (map (index b) b))
      (->> (map vector a b)
           (remove #(apply = %))
           (into {})))))






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



(defn fail-equiv? [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [fa (frequencies astr)
            fb (frequencies bstr)
            tres (reduce-kv (fn [m i a]
                     (let [b (nth bstr i)]
                       (cond (not= (fa a) (fb b)) (reduced nil)
                             (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc! m a b)
                             :else (reduced nil))))
                   (transient {})
                   (vec astr))]
        (when tres
          (persistent! tres))))))

;; new fastest
(defn eq22? [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [fa (frequencies astr)
            fb (frequencies bstr)
            tres (reduce (fn [m i]
                           (let [a (nth astr i)
                                 b (nth bstr i)]
                             (cond (not= (fa a) (fb b)) (reduced nil)
                                   (= (m a) b) m
                                   (= a b) m
                                   (nil? (m a)) (assoc! m a b)
                                   :else (reduced nil))))
                         (transient {})
                         (range (count astr)))]
        (when tres
          (persistent! tres))))))


(defn eq23? [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (mapv (frequencies astr) astr)
                  (mapv (frequencies bstr) bstr)))
      (let [tres (reduce (fn [m i]
                           (let [a (nth astr i)
                                 b (nth bstr i)]
                             (cond (= (m a) b) m
                                   (= a b) m
                                   (nil? (m a)) (assoc! m a b)
                                   :else (reduced nil))))
                         (transient {})
                         (range (count astr)))]
        (when tres
          (persistent! tres))))))


;; much faster than my original attempts
(defn equiv1? [astr bstr]
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



(defn teq3 [astr bstr]
  (if (= astr bstr)
    {}
    (when (= (count astr) (count bstr))
      (let [fa (frequencies astr)
            fb (frequencies bstr)]
        (when (empty? (sequence (remove #(= (fa (first %)) (fb (second %))))
                            (map list astr bstr)))
          (let [tres (reduce-kv (fn [m i a]
                                  (let [b (nth bstr i)]
                                    (cond (= (m a) b) m
                                          (= a b) m
                                          (nil? (m a)) (assoc! m a b)
                                          :else (reduced nil))))
                                (transient {})
                                (vec astr))]
            (when tres
              (persistent! tres))))))))


;;; why is teq2 slower than teq  ???  Also, might not handle some cases???
(defn teq2 [astr bstr]
  (if (= astr bstr)
    {}
    (when (and (= (count astr) (count bstr))
               (= (count (set astr)) (count (set bstr))))
      (let [tres (reduce-kv (fn [m i a]
                     (let [b (nth bstr i)]
                       (cond (= (m a) b) m
                             (= a b) m
                             (nil? (m a)) (assoc! m a b)
                             :else (reduced nil))))
                   (transient {})
                   (vec astr))]
        (when tres
          (persistent! tres))))))

