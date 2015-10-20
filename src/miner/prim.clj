
;; From: 	Justin Kramer <jkkramer@gmail.com>
;; Subject: 	Re: Casting numbers to ints
;; Date: 	September 19, 2011 1:16:55 PM EDT


(definterface IPrimitiveTester
  (getType [^int x])
  (getType [^long x])
  (getType [^float x])
  (getType [^double x])
  (getType [^byte x])
  (getType [^short x])
  (getType [^char x])
  (getType [^boolean x])
  (getType [^Object x]))

(deftype PrimitiveTester []
  IPrimitiveTester
  (getType [this ^int x] :int)
  (getType [this ^long x] :long)
  (getType [this ^float x] :float)
  (getType [this ^double x] :double)
  (getType [this ^byte x] :byte)
  (getType [this ^short x] :short)
  (getType [this ^char x] :char)
  (getType [this ^boolean x] :boolean)
  (getType [this ^Object x] :object))

;; We must use macros so that the primitives are available within one function.
;; If a primitive is passed out of a function, it has to be be boxed.
;; Use cast-style function to get primitive performance: (long x) or (double x).
(defmacro primitive-type [x]
  `(.getType (PrimitiveTester.) ~x))

(defmacro primitive? [x]
  `(not= :object (primitive-type ~x)))

(def unique-primitive-tester (PrimitiveTester.))
(defmacro prim-type [x]
  `(.getType ^PrimitiveTester unique-primitive-tester ~x))

(defn prim? [x]
  (not= :object (primitive-type x)))
  

(comment

  ;; Clojure 1.2

  (primitive? 1) ;=> false
  (primitive-type 1) ;=> :object
  (primitive? (Math/pow 2 2)) ;=> true
  (primitive? (* 2 (Math/pow 2 2))) ;=> false

  ;; Clojure 1.3
  
  (primitive? 1) ;=> true
  (primitive-type 1) ;=> :long
  (primitive? (Math/pow 2 2)) ;=> true
  (primitive? (* 2 (Math/pow 2 2))) ;=> true
  
  )

