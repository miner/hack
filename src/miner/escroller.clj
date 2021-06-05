(ns miner.escroller)

;; https://gist.github.com/ericnormand/04bb706198c23b8554aaf76c4edada7b

;; Write a function that takes a headline (string) and the number of characters in the LED
;; screen (integer) and returns a list of the different refreshes of the screen.

(defn scroller [headline width]
  (let [spaces (apply str (repeat width \space))
        padded (str spaces headline spaces)]
    (map #(subs padded % (+ % width)) (range (+ (count headline) (inc width))))))


;; look at cl-format for an indirect width,  200x slower!!! cl-format is not built for speed
(defn clscr [headline width]
  (let [head (clojure.pprint/cl-format nil "~vA" width headline)]
    (concat
     (map #(clojure.pprint/cl-format nil "~v@A" width (subs head 0 %))
          (range (inc (count head))))
     (map #(clojure.pprint/cl-format nil "~vA" width (subs head %))
          (range 1 (inc (count headline)))))))



;; SEM: NB edge case for empty message, probabaly should just return single space.  Fix is
;; kind of ugly so I won't bother with it:

(defn scr3 [headline width]
  (let [hcnt (count headline)
        spaces (apply str (repeat width \space))]
    (if (zero? hcnt)
      (list spaces)
      (let [padded (str spaces headline spaces)]
        (map #(subs padded % (+ % width)) (range (+ hcnt (inc width))))))))




(defn scroller-OK [msg width]
  (let [spaces (apply str (repeat width " "))
        message (str spaces msg spaces)]
    (map #(subs message % (+ % width)) (range (+ (count msg) (inc width))))))


;; slower, fiddling
(defn scroller1 [msg width]
  (let [message (apply str (into (repeat width " ") (conj (repeat width " ") msg)))]
    (map #(subs message % (+ % width)) (range (+ (count msg) (inc width))))))



(defn smoke-scroller [scroller]
  (assert (= (scroller "HEADLINE" 10)
             '("          "
               "         H"
               "        HE"
               "       HEA"
               "      HEAD"
               "     HEADL"
               "    HEADLI"
               "   HEADLIN"
               "  HEADLINE"
               " HEADLINE "
               "HEADLINE  "
               "EADLINE   "
               "ADLINE    "
               "DLINE     "
               "LINE      "
               "INE       "
               "NE        "
               "E         "
               "          ")))
  true)
