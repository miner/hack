(ns miner.lynchbell
  (:require [clojure.math.combinatorics :as mc]))

;; Puzzle for largest Lynch-Bell number
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-327-tip-always-be-decomplecting/
;; "A Lynch-Bell number is an integer that is divisible by each of its base-ten digits. The
;; digits have to be all different. ... Implement a search for the largest Lynch-Bell number."


;; Good discussion on Stack Exchange:
;; https://math.stackexchange.com/questions/2766109/biggest-lynch-bell-number#2766151
;;
;; My paraphrase:  Zero is excluded by definition.  Similarly, you can't have both 2 and
;; 5 (2x5=10).  So there are no 10 or 9 digit LBs.  A 5 and any even digit would also
;; be a problem, producing a factor of 10 and an ending 0 digit.  So 5 is out.  Leaving
;; 98764321 possible in some order.  But the sum of digits is 40 which means it can't be
;; multiple of 9.  So no eight digit LBs.
;;
;; We want to start with a 9 if possible.  That also means the LB is a multiple of 9.  So
;; the sum of the digits must be a multiple of 9.  The failing eight-digit sum was 40.  We
;; notice that 9x4 = 36 = 40 - 4, which suggests excluding 4.  That leaves a seven-digit set
;; of 9876321 (in some order) to try.
;;
;; My comments:  Excluding 0 is obviously necessary.  Excluding 5 makes sense to preserve
;; the even numbers but that's not strictly proven (by me).  Excluding 4 is based on the
;; desire to have 9 but really needs more proof than I give here.  See the link for a full
;; treatment.


(defn digits [n]
  (map #(- (long %) (long \0)) (str n)))

(defn digs->long [digs]
  (reduce #(+ (* 10 ^long %) ^long %2) 0 digs))

(defn zmod? [^long n ^long d]
  (case d
    1 true
    2 (even? n)
    (3 4 5 6 7 8 9) (zero? (rem n d))
    false))

(defn lb-digits? [n digs]
  (and (apply distinct? digs)
       (every? #(zmod? n %) digs)))

(defn lbn 
  "Given a collection of digits, returns the corresponding Lynch-Bell number or nil for
  failure."
  [digs]
  (let [n (digs->long digs)]
    (when (lb-digits? n digs)
      n)))

(defn lynch-bell? [n]
  (lb-digits? n (digits n)))

;; Note: mc/permutations guarantees lexical ordering so we seed digs with the largest digits
;; first.  That way, we test larger numbers first and stop on the first success.
(defn search-max-lb [digs]
  (first (keep lbn (mc/permutations digs))))

;; returns collection of digits in high to low orders [9...1] excluding args
(defn seed-excluding [& excluding]
  (remove (set excluding) (range 9 0 -1)))

(defn find-largest-lb [n]
  ;; starting at N digits, should be [1..7]
  (first (keep search-max-lb (map #(apply seed-excluding %)
                                  (mapcat #(mc/combinations (range 1 10) %)
                                          (range (- 9 n) 9))))))

;; From the previous discussion, we conclude that the search must start with seven digits.
;; The `seed-excluding` function returns a "seed" collection of digits with the given digits
;; excluded.
;;
;; We're pretty sure 5 should be excluded so we need to exclude one more to make
;; seven digits.  Just to be safe we have code to run through all the possible combinations
;; of 7 or fewer digits, but it will never actually execute.

(defn find-largest-lynch-bell []
  (or (first (keep search-max-lb (map seed-excluding (repeat 5) [1 2 3 4 6 7 8 9])))
      (find-largest-lb 7)))


;; Trying to keep the 9, we note that excluding the 4 gives us seven digits 9876321, whose
;; sum is 36 (a multiple of 9).  We use that as an initial seed and find success quickly.
;; I don't provide a proof that this short-cut is 100% correct, but it has the virtue of
;; yielding the right answer.  [The link to StackExchange explains why 5 and 4 should be
;; excluded.]

(defn fast-largest-lynch-bell []
  (search-max-lb [9 8 7 6 3 2 1]))


;; bonus
(defn find-all-lynch-bell-numbers 
  "Returns a sequence of all Lynch-Bell numbers in ascending order"
  []
  (sort (sequence (comp (mapcat #(mc/combinations (range 1 10) %))
                        (mapcat mc/permutations)
                        (keep lbn))
                  (range 1 8))))

(defn slow-last-lynch-bell []
  (last (find-all-lynch-bell-numbers)))


;; https://oeis.org/A115569
(def lb548 [1 2 3 4 5 6 7 8 9 12 15 24 36 48 124 126 128 132 135 162 168 175 184 216 248
            264 312 315 324 384 396 412 432 612 624 648 672 728 735 784 816 824 864 936
            1236 1248 1296 1326 1362 1368 1395 1632 1692 1764 1824 1926 1935 1962 2136 2184
            2196 2316 2364 2436 2916 3126 3162 3168 3195 3216 3264 3276 3492 3612 3624 3648
            3816 3864 3915 3924 4128 4172 4236 4368 4392 4632 4872 4896 4932 4968 6132 6192
            6312 6324 6384 6432 6912 6984 8136 8496 8736 9126 9135 9162 9216 9315 9324 9432
            9612 9648 9864 12384 12648 12768 12864 13248 13824 13896 13968 14328 14728
            14832 16248 16824 17248 18264 18432 18624 18936 19368 21384 21648 21784 21864
            23184 24168 24816 26184 27384 28416 29736 31248 31824 31896 31968 32184 34128
            36792 37128 37296 37926 38472 39168 39816 41328 41832 42168 42816 43128 43176
            46128 46872 48216 48312 61248 61824 62184 64128 68712 72184 73164 73248 73416
            73962 78624 79128 79632 81264 81432 81624 81936 82416 84216 84312 84672 87192
            89136 89712 91368 91476 91728 92736 93168 93816 98136 123648 123864 123984
            124368 126384 129384 132648 132864 132984 134928 136248 136824 138264 138624
            139248 139824 142368 143928 146328 146832 148392 148632 149328 149832 162384
            163248 163824 164328 164832 167328 167832 168432 172368 183264 183624 184392
            184632 186432 189432 192384 193248 193824 194328 194832 198432 213648 213864
            213984 214368 216384 218736 219384 231648 231864 231984 234168 234816 236184
            238416 239184 241368 243168 243768 243816 247968 248136 248976 261384 263184
            273168 281736 283416 284136 291384 293184 297864 312648 312864 312984 314928
            316248 316824 318264 318624 319248 319824 321648 321864 321984 324168 324816
            326184 328416 329184 341928 342168 342816 346128 348192 348216 348912 349128
            361248 361824 361872 362184 364128 364728 367248 376824 381264 381624 382416
            384192 384216 384912 391248 391824 392184 394128 412368 413928 416328 416832
            418392 418632 419328 419832 421368 423168 423816 427896 428136 428736 431928
            432168 432768 432816 436128 438192 438216 438912 439128 461328 461832 463128
            468312 469728 478296 478632 481392 481632 482136 483192 483216 483672 483912
            486312 489312 491328 491832 493128 498312 612384 613248 613824 613872 614328
            614832 618432 621384 623184 623784 627984 631248 631824 632184 634128 634872
            641328 641832 643128 648312 671328 671832 681432 684312 689472 732648 732816
            742896 746928 762384 768432 783216 789264 796824 813264 813624 814392 814632
            816432 819432 823416 824136 824376 831264 831624 832416 834192 834216 834912
            836472 841392 841632 842136 843192 843216 843912 846312 849312 861432 864312
            873264 891432 894312 897624 912384 913248 913824 914328 914832 918432 921384
            923184 927864 931248 931824 932184 934128 941328 941832 943128 948312 976248
            978264 981432 984312 1289736 1293768 1369872 1372896 1376928 1382976 1679328
            1679832 1687392 1738296 1823976 1863792 1876392 1923768 1936872 1982736 2137968
            2138976 2189376 2317896 2789136 2793168 2819376 2831976 2931768 2937816 2978136
            2983176 3186792 3187296 3196872 3271968 3297168 3298176 3619728 3678192 3712968
            3768912 3796128 3816792 3817296 3867192 3869712 3927168 3928176 6139728 6379128
            6387192 6389712 6391728 6719328 6719832 6731928 6893712 6913872 6971328 6971832
            7168392 7198632 7231896 7291368 7329168 7361928 7392168 7398216 7613928 7639128
            7829136 7836192 7839216 7861392 7863912 7891632 7892136 7916328 7916832 7921368
            8123976 8163792 8176392 8219736 8312976 8367912 8617392 8731296 8796312 8912736
            8973216 9163728 9176328 9176832 9182376 9231768 9237816 9278136 9283176 9617328
            9617832 9678312 9718632 9723168 9781632 9782136 9812376 9867312])
  
;; It is known
(def largest-lynch-bell 9867312)

(defn lb-test []
  (assert (every? lynch-bell? lb548))
  (assert (= (find-largest-lb 8) largest-lynch-bell))
  (assert (= (slow-last-lynch-bell) largest-lynch-bell))
  (assert (= (find-largest-lynch-bell) largest-lynch-bell))
  (assert (= (fast-largest-lynch-bell) largest-lynch-bell))
  (assert (= (find-all-lynch-bell-numbers) lb548))
  
  (println "Find largest Lynch-Bell starting at 8 digits (safe but slow)")
  (time (find-largest-lb 8))
  (println)
  (println "Last of all Lynch-Bell numbers, 7 digits")
  (time (slow-last-lynch-bell))
  (println)
  (println "Find largest Lynch-Bell number, 7 digits, excluding 5")
  (time (find-largest-lynch-bell))
  (println)
  (println "Fast, using math to limit search")
  (time (fast-largest-lynch-bell)))



;; Tested on MacBook...
;; (lb-test)
;;
;; Find largest Lynch-Bell starting at 8 digits (safe but slow)
;; "Elapsed time: 1621.554474 msecs"
;; 
;; Last of all Lynch-Bell numbers, 7 digits
;; "Elapsed time: 639.29202 msecs"
;; 
;; Find largest Lynch-Bell number, 7 digits, excluding 5
;; "Elapsed time: 51.224306 msecs"
;; 
;; Fast, using math to limit search
;; "Elapsed time: 0.096333 msecs"
;; 9867312
