(ns miner.letkey
  (:require [clojure.test :refer (deftest are)]))


(defmacro let-keys [ks-val & body]
  (assert (vector? ks-val) "let-keys first arg should be a vector")
  (assert (seq ks-val) "let-keys vector should not be empty")
  (assert (every? symbol? (butlast ks-val)))
  (let [ks# (butlast ks-val)
        val# (last ks-val)]
    `(let [{:keys [~@ks#]} ~val#]
       ~@body)))


;; For reference, defnk was in the old clojure-contrib, but dropped in the contrib reorg for 1.3
; defnk by Meikel Brandmeyer:
(comment
(defmacro defnk
 "Define a function accepting keyword arguments. Symbols up to the first
 keyword in the parameter list are taken as positional arguments.  Then
 an alternating sequence of keywords and defaults values is expected. The
 values of the keyword arguments are available in the function body by
 virtue of the symbol corresponding to the keyword (cf. :keys destructuring).
 defnk accepts an optional docstring as well as an optional metadata map."
 [fn-name & fn-tail]
 (let [[fn-name [args & body]] (name-with-attributes fn-name fn-tail)
       [pos kw-vals]           (split-with symbol? args)
       syms                    (map #(-> % name symbol) (take-nth 2 kw-vals))
       values                  (take-nth 2 (rest kw-vals))
       sym-vals                (apply hash-map (interleave syms values))
       de-map                  {:keys (vec syms)
                                :or   sym-vals}]
   `(defn ~fn-name
      [~@pos & options#]
      (let [~de-map (apply hash-map options#)]
        ~@body))))
)


;; person is a map with :last, :first, :dob
(defn pname1 [person]
  (let [{:keys [first last]} person]
    (str first " " last)))

(defn pname [person]
  (let-keys [first last person]
            (str first " " last)))

(defn pnamex [person]
  (let-keys [first last person]
    (let-keys [foo bar person]
      (str first " " last))))

(comment
(with-keys person
  [first "Foo"
   last "Bar"]
  (str first last))
)

(defn foo [mp]
  (let [{:keys [a b] :or {a 11 b 22}} mp]
    (list a b)))


(defmacro clj-when-first
  "bindings => x xs

  Same as (when (seq xs) (let [x (first xs)] body))"
  {:added "1.0"}
  [bindings & body]
  (comment
    (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector"))
  (let [[x xs] bindings]
    `(when (seq ~xs)
       (let [~x (first ~xs)]
         ~@body))))

(defmacro when-first1
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (assert (vector? bindings) "a vector for its binding")
  (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(let [xs# (seq ~xs)
           fst# (first xs#)
           test# (and xs# true)]
       (when test#
         (let [~x fst#]
           ~@body)))))

(defmacro when-first2
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (assert (vector? bindings) "a vector for its binding")
  (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(let [xs# (seq ~xs)
           fst# (first xs#)]
       (when xs#
         (let [~x fst#]
           ~@body)))))

(defmacro when-first3
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (comment
    ;; private macro in core.clj
    (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector"))
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
           ~@body))))

(defn slow [n] (println "sleeping" n) (flush) (Thread/sleep (* n 10)) (range n 100))

(defn huge [n] (range n 1e9))


;; Not perfect but good enough for testing
(defn free-memory []
  (quot (.freeMemory (Runtime/getRuntime)) 1024))

(defn prn-mem [& args]
  (System/gc)
  (print " free " (free-memory) " ")
  (apply println args)
  (flush))

(defn garbage []
  (print "making garbage")
  (let [chunk (make-array Byte/TYPE (* 800 1024 1024))]
    (prn-mem)
    chunk))

;; https://groups.google.com/forum/?fromgroups=#!topic/clojure/FLrtjyYJdRU
;; https://groups.google.com/d/msg/clojure/Xmu3pUMgiJk/Lx1Sq462h4wJ

(defn locals-clearing-test []
  (let [a (garbage)
        b (when a (garbage))
        c (when b (garbage))
        d (when c (garbage))
        e (when d (garbage))
        f (when e (garbage))
        g (when f (garbage))
        h (when g (garbage))]
    (println "OK")))

(defn oom-test []
  (let [a (garbage)
        b  (garbage)
        c   (garbage)
        d   (garbage)
        e  (garbage)
        f  (garbage)
        g   (garbage)
        h   (garbage)]
    (println "surprisingly OK")))

(defn big []
  (prn-mem "pre-big")
  (let [result (doall (range 4 1e6))]
    (prn-mem "post-big")
    result))

(defn twf []
  (prn-mem "before")
  (when-first [x (big)]
      (prn-mem "top")
      (println (+ x x))
      (prn-mem "bot"))
  (prn-mem "out"))


(defn twf3 []
  (prn-mem "before")
  (when-first3 [x (big)]
      (prn-mem "top")
      (println (+ x x))
      (prn-mem "bot"))
  (prn-mem "out"))


(defn rindex
  "Returns the index of s relative to the start when i is positive, or
  relative to the end when i is negative."
  {:added "1.5"
   :static true}
  [^String s i] (+ i (if (< i 0) (count s) 0)))

(defn rindex [coll i]
  "Returns an index into coll relative to the start when i is positive, or
  relative to the end when i is negative."
  (if (neg? i) (+ (count coll) i) i))


(defn mapcall
  ([fs] (map #(%) fs))
  ([fs as] (map #(% %2) fs as))
  ([fs as bs] (map #(% %2 %3) fs as bs))
  ([fs as bs & more] (map #(apply % %2 %3 %4) fs as bs more)))



(defn wrap-args
  "Return a function like f but with the args transformed by g
   (which should take the same args as f and return a seq of
   replacement values to be applied to f."
  ([f] f)
  ([f g] (fn [& xs] (apply f (g xs)))))

(defn subsr
  ([s begin] (subs s (rindex s begin)))
  ([s begin end] (subs s (rindex s begin) (rindex s end))))


(defn bad-wrap-args
  "Return a function like f but with the positional args wrapped by the given functions.  Use identity to pass the original arg."
  ([f] f)
  ([f fa] (comp f fa))
  ([f fa fb] (fn [a b] (f (fa a) (fb b))))
  ([f fa fb & fcs] (fn [a b & cs] (apply f (fa a) (fb b) (map #(% %2) fcs cs)))))

