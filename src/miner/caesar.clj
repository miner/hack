(ns miner.caesar)

;; http://clojurebridge.github.io/community-docs/docs/exercises/caesar-cipher/
;; I don't love their code.  Should make a suggestion or file a github issue for them.

(defn caesar-cipher [words offset]
  "Assumes offset >=0, words entirely lowercase English characters or spaces"
  (let [alphabet-chars (map char "abcdefghijklmnopqrstuvwxyz")
        alphabet-shifted (->> (cycle alphabet-chars) (take 100) (drop offset))
        shifted-map (-> (zipmap alphabet-chars alphabet-shifted)
                        (assoc \space \space))]
    (apply str (map shifted-map (map char words)))))
 

(comment
  (caesar-cipher "hello" 1)
  ; => "ifmmp"
  )

;; Should take advantage of (seq "foo") returns seq of chars.  No need for (map char "foo")
;; If you want to be explicit for pedagogical reasons, just call seq.
;; Should (mod offset 26) to allow more flexibilty, negatives
;; Should pass through non-lower-alpha chars, not just \space.  Second arg "not-found" to
;; map access.
;; One could argue that you really only want one-case support.

(defn caesar1
  ([message] (caesar1 message 13))
  ([message offset]
   (let [lowers "abcdefghijklmnopqrstuvwxyz"
         shifted-map (zipmap lowers (drop (mod offset (count lowers)) (cycle lowers)))]
     (apply str (map #(shifted-map % %) message)))))

;;;;; OTHER IDEAS and extensions


;; Adding Cap support, any int offset (mod 26), and pass through for non-alphas
;; shifted-map with not-found option to get char itself as default
;; default single arity, offset 13 (traditionally, you call cipher twice to recover message)
(defn caesar2
  ([message] (caesar2 message 13))
  ([message offset]
   (let [lowers (seq "abcdefghijklmnopqrstuvwxyz")
         uppers (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
         shifted-map (merge (zipmap lowers (drop (mod offset (count lowers)) (cycle lowers)))
                            (zipmap uppers (drop (mod offset (count uppers)) (cycle uppers))))]
    (apply str (map #(shifted-map % %) message)))))


(defn caesar-char [ch offset]
  (let [ic (long ch)
        ia (long \a)
        iz (long \z)
        iA (long \A)
        iZ (long \Z)]
    ;; (assert (= 26 (inc (- iz ia)) (inc (- iZ iA))))
    (cond (<= ia ic iz)   (char (+ ia (mod (+ (- ic ia) offset) 26)))
          (<= iA ic iZ)   (char (+ iA (mod (+ (- ic iA) offset) 26)))
          :else ch)))

(defn caesar-good
  ([message] (caesar-good message 13))
  ([message offset]
   (apply str (map #(caesar-char % offset) message))))

;; the classic default is 13, so calling it twice returns original
(defn caesar-classic
  ([message] (caesar-classic message 13))
  ([message offset]
   (let [cch (fn [ch]
               (let [ic (long ch)
                     ia (long \a)
                     iz (long \z)
                     iA (long \A)
                     iZ (long \Z)]
                 ;; (assert (= 26 (inc (- iz ia)) (inc (- iZ iA))))
                 (cond (<= ia ic iz)   (char (+ ia (mod (+ (- ic ia) offset) 26)))
                       (<= iA ic iZ)   (char (+ iA (mod (+ (- ic iA) offset) 26)))
                       :else ch)))]
     (apply str (map cch message)))))



;; faster, surprisingly
(defn tca
  ([message] (tca message 13))
  ([message offset]
   (let [cha (fn [^long aint ^long i] (char (+ aint (mod (+ (- i aint) offset) 26))))
         cch (fn [ch]
               (let [ic (long ch)
                     ia (long \a)
                     iz (long \z)
                     iA (long \A)
                     iZ (long \Z)]
                 ;; (assert (= 26 (inc (- iz ia)) (inc (- iZ iA))))
                 (cond (<= ia ic iz)   (cha ia ic)
                       (<= iA ic iZ)   (cha iA ic)
                       :else ch)))]
     (transduce (map cch) str message))))


;; `rfstr` is a "reducing step function" for transduce, no arg for init, expects
;; chars, returns final result string

(defn rfstr
  ([] (StringBuilder.))
  ([sb] (.toString ^StringBuilder sb))
  ([sb ch] (.append ^StringBuilder sb ^char ch)))


;; fastest
(defn caesar
  ([message] (caesar message 13))
  ([message offset]
   (let [rotch (fn [ch]
                 (let [ic (long ch)
                       ia (long \a)
                       iz (long \z)
                       iA (long \A)
                       iZ (long \Z)]
                   (cond (<= ia ic iz)  (char (+ ia (mod (+ (- ic ia) offset) 26)))
                         (<= iA ic iZ)  (char (+ iA (mod (+ (- ic iA) offset) 26)))
                         :else ch)))]
     (transduce (map rotch) rfstr message))))



;; looks nice but slower
(defn tca1
  ([message] (tca1 message 13))
  ([message offset]
   (let [ia (long \a)
         iz (long \z)
         iA (long \A)
         iZ (long \Z)
         rot (fn [^long aint ^long i] (+ aint (mod (+ (- i aint) offset) 26)))
         ciaz (fn [i] (if (<= ia i iz) (rot ia i) i))
         ciAZ (fn [i] (if (<= iA i iZ) (rot iA i) i))]
     (transduce (comp (map long) (map ciaz) (map ciAZ) (map char)) rfstr  message))))

 




(defn smoke-caesar [cstr]
  (assert (every? #(= (cstr (cstr %)) %) ["foobar" "FooBar" "baz23" "_BAZ$BB?"]))
  (assert (every? #(= (cstr (cstr %)) %) ["Well, isn't that special?", "Can you spell Antidisestablishmentarianism? Maybe with a clue."]))
  (assert (every? #(= (cstr (cstr "FooBar9" %) (- %)) "FooBar9") (range 100)))

  true)





;; not faster, but handles Caps
(defn caesar-char-case [ch offset]
  (let [ic (long ch)
        ia (long \a)
        iA (long \A)]
    (case ch
      (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)
      (char (+ ia (mod (+ (- ic ia) offset) 26)))

      (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z)
      (char (+ iA (mod (+ (- ic iA) offset) 26)))

      ch)))

(defn caesarc
  ([message] (caesarc message 13))
  ([message offset]
   (apply str (map #(caesar-char-case % offset) message))))

(let [ia (long \a)
      iA (long \A)
      iz (long \z)
      iZ (long \Z)]
(defn caesar-ch [ch offset]
  (let [ic (long ch)]
    (cond (<= ia ic iz) (char (+ ia (mod (+ (- ic ia) offset) 26)))
        (<= iA ic iZ) (char (+ iA (mod (+ (- ic iA) offset) 26)))
        :else ch))))

(defn caesarch
  ([message] (caesarch message 13))
  ([message offset]
   (apply str (map #(caesar-ch % offset) message))))
