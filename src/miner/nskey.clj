(ns miner.nskey)




(defn single-sym [sym]
  (symbol (subs (peek (clojure.string/split (str sym) #"[.]")) 0 1)))

(defn final-segment [sym]
  (let [segments (clojure.string/split (str sym) #"[.]")]
    (when (> (count segments) 1)
      (symbol (peek segments)))))

(defn final-segment [sym]
  (let [segments (clojure.string/split (str sym) #"[.]")]
    (when (> (count segments) 1)
      (symbol (peek segments)))))





(defn find-short-name [parent-ns qualified-name]
  (let [alias-names (set (map str (keys (ns-aliases parent-ns))))
        segments (clojure.string/split (str qualified-name) #"[.]")
        final (peek segments)
        fcnt (count final)
        shorten (fn [n extra] (let [sname (and (>= fcnt n) (str (subs final 0 n) extra))]
                                (and sname (not (get alias-names sname)) (symbol sname))))
        shorten+ (fn [n] (first (map #(shorten n %) (range 1 10))))]

    (or (shorten 1 "") 
        (shorten 3 "") 
        (shorten 2 "")
        (and (> (count segments) 1) (not (get alias-names final)) (symbol final))
        (and (> (count segments) 1) (let [abc (apply str (map first segments))]
                                      (and (not (get alias-names abc))
                                           (symbol abc))))
        (shorten+ 1)
        (shorten+ 2)
        (shorten+ 3)  )))


(defn abbrev-ns
  ([qualified-name]   (abbrev-ns *ns* qualified-name (find-short-name *ns* qualified-name)))
  ([qualified-name short-name]  (abbrev-ns *ns* qualified-name short-name))
  ([parent-ns qualified-name short-name]
  ;; alias qualified-name as short-name
   (if (get (ns-aliases parent-ns) short-name)
     (throw (ex-info (str "Namespace alias " short-name " already exists in Namespace "
                          (ns-name parent-ns))
	  		 		  {:conflicting-alias short-name :ns parent-ns}))
	  (let [qns (create-ns qualified-name)
            save (ns-name *ns*)
            parent (ns-name parent-ns)]
        (in-ns parent)
	    (alias short-name qns)
        (in-ns save)
        short-name))))

