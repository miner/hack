;; http://dev.clojure.org/jira/browse/CLJ-2094

;; The protocol is stored in a var Game so its value changes as extensions are made.  The
;; original bug reported complained that there's a difference between ::game1 and ::game2.
;; Yes, because ::game2 captures the value of the Game protocol at definition-time, whereas
;; ::game1 creates a closure with a reference to the var (so it works later when the
;; protocol is updated.)  This is just normal Clojure evaluation rules, not a bug.  However,
;; the error message you get is misleading so maybe they'll fix that.

;; I played around with trying to specially handle var references, but none of those ideas
;; were any good.  Better to just make a normal predicate function with `defn` which handles
;; the var reference the right way.

(ns miner.specproto
  (:require [clojure.spec.alpha :as s]
            [clojure.main :as m]))

(defprotocol Game
  (move [game]))

(s/def ::game1 #(satisfies? Game %))
(s/def ::game2 (partial satisfies? Game))

(defn my-game? [x] (satisfies? Game x))

(s/def ::game3 my-game?)

(s/def ::game4 (partial satisfies? Game))

(defn vref [maybe-var]
  (if (var? maybe-var)
    @maybe-var
    maybe-var))

(defn fvargs [f]
  (fn [& args]
    (apply f (map vref args))))


(defn vsat? [proto-or-var x]
  (satisfies? (vref proto-or-var) x))

(s/def ::game4a (partial (fvargs satisfies?) #'Game))

(defmacro partialm
  ([f] `(partial ~f))
  ([f a] (if (symbol? a)
           `(fn [& args#] (apply ~f (vref (var ~a)) args#))
           `(partial ~f ~a)))
  ([f a b] (let [a (if (symbol? a) `(vref (var ~a)) a)
                 b (if (symbol? b) `(vref (var ~b)) b)]
             `(fn [& args#] (apply ~f ~a ~b args#)))))

(s/def ::game4b (partialm satisfies? Game))


(defrecord Foo [])

(extend-type Foo
  Game
  (move [game]))

(s/def ::game5 (partial satisfies? miner.specproto/Game))

(s/def ::game6 (partial satisfies? Game))


;; Use only for debugging.  Very expensive at runtime.
(defmacro current-fn-name []
  "Returns a string, the name of the current Clojure function"
  `(m/demunge (.getClassName ^StackTraceElement (first (.getStackTrace (Throwable.))))))


(defmacro debug
  ([]  `(println "# Debug" (current-fn-name)))
  ([a] `(let [x# ~a]
            (println '~a "=>" x#)
            x#))
  ([a & forms]
   `(do (debug ~a)
        ~@(map #(list 'debug %) forms))))


(defn my-test []
  (let [foo (->Foo)]
    (debug)
    (debug
     (satisfies? Game foo)
     ((partial satisfies? Game) foo)
     (s/valid? ::game1 foo)
     (s/valid? ::game2 foo)
     (s/valid? ::game3 foo)
     (s/valid? ::game4 foo)
     (s/valid? ::game4a foo)
     (s/valid? ::game4b foo)
     (s/valid? ::game5 foo)
     (s/valid? ::game6 foo)
     )))


