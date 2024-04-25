(ns miner.base32
  (:require [clojure.string :as str]))

;;; https://www.crockford.com/base32.html
;;; Base 32 is a textual 32-symbol notation for expressing numbers in a form that can be
;;; conveniently and accurately transmitted between humans and computer systems. It can be
;;; used for out of band communication of public keys.

;;; Avoids a few potentially confusing characters (to humans).  Basically, 5 bits per char.
;;; My implementation implements the recommended (mod n 37) checksum character.


(def ichar32 (vec "0123456789ABCDEFGHJKMNPQRSTVWXYZ"))

(assert (= 32 (count ichar32)))

(def checksum-extras "*~$=U")

(def ichar37 (into ichar32 checksum-extras))

(assert (= 37 (count ichar37)))

(def extra-istr {1 "LlIi" 0 "Oo"})

(def extci (reduce-kv (fn [m k s] (reduce (fn [m c] (assoc m c k)) m s)) {} extra-istr))

(def chari (into extci (map-indexed (fn [i c] [c i])) ichar32))

(def chari32 (reduce (fn [m c] (assoc m c (m (char (- (int c) 32)))))
                     chari
                     "abcdefghjkmnpqrstvwxyz"))

(def chki37 (into (assoc chari32 \u 36)
                   (map-indexed #(vector %2 (+ % 32)))
                   checksum-extras))



;;; ch encode 5 bits (base 32)
;;; last char is the checksum (rem n 37)
(defn check32 [i]
  (if (<= 0 i 31)
    i
    (throw (ex-info "Base32 Illegal Bits" {:bit5 i}))))


(defn decode32 [s]
  (let [lasti (dec (count s))
        chksum (chki37 (nth s lasti))]
    (transduce (comp (map chari32) (map check32))
               (fn ([n i] (+ (bit-shift-left n 5) i))
                 ([n] (if (= (rem n 37) chksum)
                        n
                        (throw (ex-info "Base32 Checksum Error"
                                        {:value s
                                         :checksum chksum
                                         :calculated (rem n 37)})))))
               0
               (subs s 0 lasti))))

(defn base32 [n]
  (assert (or (zero? n) (pos-int? n)))
  (loop [cs (list (ichar37 (rem n 37))) n n]
    (let [cs (conj cs (ichar32 (bit-and 0x1F n)))
          n (unsigned-bit-shift-right n 5)]
      (if (zero? n)
        (str/join cs)
        (recur cs n)))))




;;; SEM: I wonder what it would look like if you intentionally encoded things into the worst
;;; possible confusable symbols as a way of making human reading or speaking difficult.
;;; Probabaly not a good idea.  Just for fun.

;;; https://gajus.com/blog/avoiding-visually-ambiguous-characters-in-ids

;;; O / 0 - The letter O and the number 0 can look very similar, especially in fonts where the number zero isn’t slashed or dotted.
;;;
;;; I / l / 1 / 7 - The letter I (uppercase i), lowercase l (L), the number 1, and the number 7 1 can be indistinguishable in many types of print and handwriting.
;;;
;;; 5 / S - In some fonts, the number 5 and the letter S can appear quite similar.
;;;
;;; 2 / Z - These can also be confused with each other, particularly in handwritten forms.
;;;
;;; 8 / B - These characters might be mixed up when poorly written or in certain stylized fonts.
;;;
;;; 6 / G - These characters can be confused in some fonts.
;;;
;;; 9 / q / g - These characters can be confused in some fonts.

;;; Maybe for some of the symbols, you could randomly encode with several ambiguous chars.

;;; call it: 8l6LI or "Bigly" encoding.
;;; only uses


;;; zero char \O means nothing so it can be randomly inserted -- not implemented
;;; otherwise a variation on hex code

#_ (def bigly "OIl175S2Z8B6G9qg")

(def bigv [\O \1 \2 \5 \6 \7 \8 \9 \B \G \I \S \Z \g \l \q])

(def hexstr "0123456789abcdef")

(def ihex (vec hexstr))

(def hexbig (zipmap hexstr bigv))

(def bigh (zipmap bigv (range 16)))

(defn ibigly [n]
  (str/join (cons "0y" (map hexbig (Long/toHexString n)))))

(defn biglyi [bg]
  (when (= "0y" (subs bg 0 2))
    (reduce (fn [i h] (+ (* 16 i) h))
            0
            (map bigh (subs bg 2)))))


#_ (def hxh (zipmap "0123456789abcdef" (range 16)))

#_
(defn hexi [hx]
  (reduce (fn [i h] (+ (* 16 i) h))
          0
          (map hxh hx)))
