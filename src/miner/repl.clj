
(ns miner.repl
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

;; https://gist.github.com/2049970
;; ghoseb via cgrand

;; bug -- doesn't notice name collisions for methods in multiple interfaces
(defn scaffold
  "Print the ancestor method signatures of a given interface."
  [iface]
  (doseq [[iface methods] (->> ^Class iface
                               .getMethods
                               (map (fn [^java.lang.reflect.Method meth] 
                                      (vector (.getName (.getDeclaringClass meth))
                                              (symbol (.getName meth))
                                              (count (.getParameterTypes meth)))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into ['this] (map #(symbol (str "arg" %)) (range 1 (inc argcount))))))))))


;; inspired by runa-dev/homeless, but worth unrolling a bit
;; I like the explicit (seq m).
;; But really you should only use kwargs for REPL level command functions, not normal API
(defn apply-kw
  "Like apply but the last arg is a map containg keyword args that are unrolled before
  applying f"
  ([f] (f))
  ([f m] (apply f (apply concat (seq m))))
  ([f a m] (apply f a (apply concat (seq m))))
  ([f a b m] (apply f a b (apply concat (seq m))))
  ([f a b c & args+m] (apply f a b c (concat (butlast args+m) 
                                             (apply concat (seq (last args+m)))))))


;; hacked version of clojure.repl/find-doc which does too much
(defn rdoc
  "Prints documentation for any var whose name matches `regex`."
  [regex]
    (let [re (re-pattern regex)
          ms (concat (mapcat #(sort-by :name (map meta (vals (ns-interns %))))
                             (all-ns))
                     (map @#'clojure.repl/namespace-doc (all-ns))
                     (map @#'clojure.repl/special-doc (keys @#'clojure.repl/special-doc-map)))]
      (doseq [m (filter (fn [m] (and (:doc m) (re-matches re (str (:name m))))) ms)]
        (#'clojure.repl/print-doc m))))


;; Not perfect, but useful for Clojure functions defined with source code.  Not so much for
;; anonymous functions.
(defn as-symbol [x]
  (if (fn? x)
    (symbol (repl/demunge (str/replace-first (pr-str (type x)) #"__\d+$" "")))
    (let [sym (symbol x)]
      (if (qualified-symbol? sym)
        sym
        (if-let [vr (resolve sym)]
          (symbol vr)
          sym)))))

;; opendiff is Mac-only.  On other platforms, subsitute your diff tool.

;; hard-coding the /tmp files saves some churn in /tmp
(defn fdiff [fn1 fn2]
  (let [fn1 (as-symbol fn1)
        fn2 (as-symbol fn2)]
    (spit "/tmp/fdiff-1.clj" (repl/source-fn fn1))
    (spit "/tmp/fdiff-2.clj" (repl/source-fn fn2))
    (sh/sh "/usr/bin/opendiff" "/tmp/fdiff-1.clj" "/tmp/fdiff-2.clj")
    (list 'fdiff fn1 fn2)))

