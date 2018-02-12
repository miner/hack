;; Survey of Rounding Implementations in Go
;; https://www.cockroachlabs.com/blog/rounding-implementations-in-go/

(ns miner.rounding)

(defn round ^Long [^Double r]
 (Math/round r))

(def negZero -0.0)

;; original code called this negZero in result
;; so I guess their language had an integer negative zero
;; Or maybe their round function always returned a float/double, and didn't go to an integer
(def longZero 0)

;; ISSUE:  Java Math/round rounds toward positive infinity
;; but Go wants to round towards -infinity for negs????

(defn smoke-test []
  (doseq [[val expected] 
          [[-0.49999999999999994, longZero] ; -0.5+epsilon
           ;; [-0.5, -1]
           [-0.5000000000000001, -1] ; -0.5-epsilon
           [0, 0],
           [0.49999999999999994, 0], ; 0.5-epsilon
           [0.5, 1],
           [0.5000000000000001, 1],                         ; 0.5+epsilon
           [1.390671161567e-309, 0],                        ; denormal
           ;;[2.2517998136852485e+15, 2.251799813685249e+15], ; 1 bit fraction
           ;;[4.503599627370497e+15, 4.503599627370497e+15],  ; large integer
           ;; [math.Inf(-1), math.Inf(-1)],
           ;; [math.Inf(1), math.Inf(1)],
           ;; [math.NaN(), math.NaN()],
           ;; [negZero, longZero],
           [-0.0, 0]
           [0.0, 0]]]
    (assert (= (round val) expected) (str "Failed on " val)))
  true)


