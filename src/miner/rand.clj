
;; http://v8project.blogspot.com/2015/12/theres-mathrandom-and-then-theres.html

;; xorshift128+
(ns miner.rand)

;; SEM: this code is experimental and untested.  Not ready for prime time.


;; int64_t state0 = 1;
;; int64_t state1 = 2;
;; int64_t xorshift128plus() {
;;  uint64_t s1 = state0;
;;  uint64_t s0 = state1;
;;  state0 = s0;
;;  s1 ^= s1 << 23;
;;  s1 ^= s1 >> 17;
;;  s1 ^= s0;
;;  s1 ^= s0 >> 26;
;;  state1 = s1;
;;  return state0 + state1;
;; }


;; not perfectly safe to use two independent atoms
;; but probably doesn't matter if there's a small window for two threads to interleave updates

(def state0 (atom 1))
(def state1 (atom 2))

(defn myrand []
  (let [s1 (long @state0)
        s0 (long @state1)
        s1a (long (bit-xor s1 (bit-shift-left s1 (long 23))))
        s1b (long (bit-xor s1a (unsigned-bit-shift-right s1a (long 17))))
        s1c (long (bit-xor s1b s0))
        s1d (long (bit-xor s1c (unsigned-bit-shift-right s0 (long 26))))]
    (reset! state1 s1d)
    (reset! state0 s0)
    (unchecked-add s0 s1d)))

    

;; https://en.wikipedia.org/wiki/Xorshift

;; int64_t s[2];
;; 
;; int64_t xorshift128plus(void) {
;;    uint64_t x = s[0];
;;    uint64_t const y = s[1];
;;    s[0] = y;
;;    x ^= x << 23; // a
;;    s[1] = x ^ y ^ (x >> 17) ^ (y >> 26); // b, c
;;    return s[1] + y;
;; }


(def s (long-array [1 2]))

;; without locking
(defn xorshift+ []
    (let [s (longs s)
          x (long (aget s (long 0)))
          y (long (aget s (long 1)))
          x1 (long (bit-xor x (bit-shift-left x (long 23))))
          s1 (long (aset s 1 (bit-xor x1 y
                                (bit-shift-right x1 (long 17))
                                (bit-shift-right y (long 26)))))]
      (aset s (long 0) y)
      (unchecked-add s1 y)))

(defn safe-xorshift+ []
  (locking s
    (xorshift+)))



;; let around defn localized state without var for s
;; also avoids need to cast s with (longs s)

;; without locking
(let [s (long-array [1 2])]
  (defn lxorshift+ []
    (let [x (long (aget s (long 0)))
          y (long (aget s (long 1)))
          x1 (long (bit-xor x (bit-shift-left x (long 23))))
          s1 (long (aset s 1 (bit-xor x1 y
                                      (unsigned-bit-shift-right x1 (long 17))
                                      (unsigned-bit-shift-right y (long 26)))))]
      (aset s (long 0) y)
      (unchecked-add s1 y))))



;; for testing
#_  (time (filter (fn [[k v]] (when (> v 1) [k v])) (frequencies (repeatedly 1e6 xorshift+))))

;; expecting empty list result (no dupes)


;; compare to rand-int (32-bit results, not long)
;; about the same speed as above but maybe most of the time is in the freq and filt
#_ (time (filter (fn [[k v]] (when (> v 1) [k v]))
                 (frequencies (repeatedly 1e6 #(rand-int Integer/MAX_VALUE)))))
;; lots of dupes
