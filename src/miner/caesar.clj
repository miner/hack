
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
    (cond (<= ia ic iz)   (char (+ ia (mod (+ (- ic ia) offset) (- iz ia))))
        (<= iA ic iZ)   (char (+ iA (mod (+ (- ic iA) offset) (- iZ iA))))
        :else ch)))

;; fastest and reasonably clear
(defn caesar
  ([message] (caesar message 13))
  ([message offset]
   (apply str (map #(caesar-char % offset) message))))



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
