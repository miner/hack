(ns miner.deftools)

;; allow doc-string to come before the args (like CL)
(defmacro defun [name args & forms]
  (if (and (string? (first forms))
           (vector? args)
           (> (count forms) 1))
    `(defn ~name ~(first forms) ~args ~@(rest forms))
    `(defn ~name ~args ~@forms)))

;; SEM: hack
;; inverse of resolve  
(defn var->symbol
  "Return symbol used to name Var v."
  [^clojure.lang.Var v]
  (when-let [sy (. v sym)]
    (symbol (name (ns-name (. v ns))) (name sy))))

