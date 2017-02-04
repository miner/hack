(ns miner.deftools)

;; allow doc-string to come after the args (like CL)
(defmacro defun [name args doc-str & forms]
  (if (and (string? doc-str)
           (vector? args)
           (seq forms))
    `(defn ~name ~doc-str ~args ~@forms)
    `(defn ~name ~args ~doc-str ~@forms)))

;; better to put this sort of check into a linter



;; SEM: hack
;; inverse of resolve  
(defn var->symbol
  "Return symbol used to name Var v."
  [^clojure.lang.Var v]
  (when-let [sy (. v sym)]
    (symbol (name (ns-name (. v ns))) (name sy))))



;; defer execution of body until called for first time, then cache result and return that
;; like a no-arg version of memoize
(defmacro defer [& body]
  `(let [res# (atom nil)]
     (fn []
       (or
         (deref res#)
         (reset! res# ((fn [] ~@body)))))))


;; SEM BUGGY
(defmacro defer2 [& body]
  `(let [calc# (fn [] ~@body)
         trigger# (fn [x] (if (fn? x) (x) x))]
     (trigger# calc#)))

