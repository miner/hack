
;;; This stuff is old and obsolete.  Clojure has changed a lot internally since 1.0.

;;; My preferred replacement is updated in halfbaked.clj.

;;; No Namespace here.  We're not using this anywhere.  Preserved for historic interest.


;; replacements for demangle
;; faster but ugly

;; See also clojure.main/demunge


;; See  clojure.lang.Compiler/CHAR_MAP for the official list of conversions
;; Some don't happen in normal code so I elided them.
(def demangle-replacements
  (array-map "_QMARK_" "?"
             "_BANG_" "!"
             "_STAR_" "*"
             "_GT_" ">"
             "_EQ_" "="
             "_PLUS_" "+"
             "_LT_" "<"
             "_SLASH_" "/"
             "_AMPERSAND_" "&"
             "_TILDE_" "~"
             ;; keep underbar last
             "_" "-"))

;; see also clojure.main/demunge

;; a faster but ugly version is in demangle.clj
(defn ^String demangle
  "Demangle a clojure identifier name"
  [^String s]
  (reduce-kv str/replace s demangle-replacements))

(defn compiled-fn-name
  "returns the simple name (a string) for the given function f as determined by the compiler"
  [f]
  (let [f (if (var? f) (var-get f) f)
        compiled-name (when (fn? f) (str f))
        fname (second (first (re-seq #"[$](.*)@" compiled-name)))]
    (if fname
      (demangle fname)
      compiled-name)))


;; original:
;; http://groups.google.com/group/clojure/browse_thread/thread/234ac3ff0a4b6b80?pli=1
;; but slightly changed for Clojure updates since 1.0

;; See also clojure.repl/demunge for the official way to do this now.

(defn demangle-class-name
  "Given the name of a class that implements a Clojure function, returns the function's
   name in Clojure. Note: If the true Clojure function name
   contains any underscores (a rare occurrence), the unmangled name will
   contain hyphens at those locations instead."
  [class-name]
  (demangle (clojure.string/replace class-name #"^(.+)\$([^@]+)(|@.+)$" "$1/$2")))

;; only appropriate for debugging
(defmacro current-fn-name []
  "Returns a string, the name of the current Clojure function"
  `(-> (Throwable.) .getStackTrace first .getClassName demangle-class-name))

(defmacro not-implemented []
  `(throw (Error. (str "fn " (current-fn-name) " not implemented"))))


;; horrible loop doing two things at once
(defn ^String demangle2
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0 under -1 reps demangle-replacements]
      (let [under (if (neg? under) (.indexOf s "_" start) under)]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          (let [[^String code ^String rep] (first reps)]
            (if (.startsWith s code under)
              (do (.append (.append sb s (int start) (int under)) rep)
                  (recur (long (+ under (.length code)))
                         -1
                         demangle-replacements))
              (recur (long start) (long under) (rest reps)))))))))

;; slightly slower, but a little better
(defn ^String demangle3
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0]
      (let [under (.indexOf s "_" (int start))]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          (recur (let [[^String code ^String rep] 
                       (first (filter #(.startsWith s ^String (key %) under)
                                      demangle-replacements))]
                   (.append (.append sb s (int start) (int under)) rep)
                   (long (+ under (.length code))))))))))

;; Fastest
(defn ^String demangle4
  "Demangle a clojure identifier name"
  [^String s]
  (let [len (.length s)
        sb (StringBuilder. len)]
    (loop [start 0]
      (let [under (.indexOf s "_" (int start))]
        (if (neg? under)
          (.toString (.append sb s (int start) (int len)))
          ;; crazy usage of reduce-kv and reduced short-circuit with side-effects
          (recur (long (reduce-kv (fn [_ ^String code ^String rep]
                                    (if (.startsWith s code under)
                                      (do (.append (.append sb s (int start) (int under)) rep)
                                          (reduced (+ under (.length code))))
                                      under))
                                  under
                                  demangle-replacements))))))))
