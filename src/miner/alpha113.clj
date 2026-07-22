(ns miner.alpha113)

;;; https://clojure.org/news/2026/07/14/clojure-1-13-alpha4

;;; my comments on Clojure 1.13.0-alpha4

;;; https://ask.clojure.org/index.php/15179/destructuring-with-keys-and-or

;;; I'm trying Clojure 1.13.0-alpha4. I have a problem with :keys! and :or. The first example works as expected with the old :keys. The second example gives me a syntax error when I switch to the new :keys!. The thirds example works when I avoid the required key in the defaults.

(comment
 (let [{:keys [a] :or {:a 1}} {:a 2}] a)
 ;;=> 2
    
(let [{:keys! [a] :or {:a 1}} {:a 2}] a)
;;Syntax error macroexpanding let at (REPL:1:1).
;;Can't supply default value for required key: :a

(let [{:keys! [a] :or {:b 1}} {:a 2}] a)
;;=> 2

)

;;; Alex Miller writes:  a is now bound and thus :a is required. Because of this, there is
;;; no possibility to use :a from :or in :defaults or :select, so this is now an error.
;;;
;;; SEM: I had interpreted the error message as saying that the Clojure compiler couldn't do
;;; something because of the key.  Actually, the message is telling me that *I* can't do
;;; that.

;;; I didn't want to belabor the point online.  Now, after thinking about it some more, I
;;; think the compiler should simply ignore any :or defaults for the required :keys!.  We
;;; know that the runtime will check for the required keys before it would ever need to
;;; consider a possible default.  So the defaults are superfluous, but that's not really a
;;; syntax error.  It's more like sloppy thinking.  The main point is that we don't need the
;;; default at runtime -- we're going to have the real key there.  If the key is not in the
;;; data value, that's the error.  And it would be the error whether or not we specified a
;;; default value.  The compiler error is not really helping us fix anything.  It's just
;;; telling us, there's no *need* for an :or default for a required key.  A sufficiently
;;; smart compiler could do "dead default optimization" and ignore the superfluous defaults.
;;; No need to generate code for the default.  Maybe it's worth a warning or a case for a
;;; linter like clj-kondo.




(comment

  ;; using :keys to generate code but it would be similar with :keys!
  (macroexpand '(let [{:keys [a] :or {:a (expensive)} } {:a 2}] a))

  ;;; pprinted
  (let*
   [default__3677  (expensive)
    map__3675  {:a 2}
    map__3675  (if (clojure.core/seq? map__3675)
                 (if (clojure.core/next map__3675)
                   (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                    (clojure.core/to-array map__3675))
                   (if (clojure.core/seq map__3675)
                     (clojure.core/first map__3675)
                     clojure.lang.PersistentArrayMap/EMPTY))
                 map__3675)
    a (clojure.core/get map__3675 :a default__3677)]
   a)

  )

;;; :keys! changes c.c/get to c.c/req!   However, the compiler gives an error in alpha4 if
;;; your required keys are included in the :or defaults.

;;; So I'm suggesting that the compiler collect the required :keys! and remove them from the
;;; :or defaults so it doesn't have to generate the call to (expensive) at all.  In this
;;; case, it would be like:

(comment

  (macroexpand '(let [{:keys! [a] :or {} } {:a 2}] a))

  (let*
   [map__3698  {:a 2}
    map__3698  (if (clojure.core/seq? map__3698)
                 (if (clojure.core/next map__3698)
                   (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                    (clojure.core/to-array map__3698))
                   (if (clojure.core/seq map__3698)
                     (clojure.core/first map__3698)
                     clojure.lang.PersistentArrayMap/EMPTY))
                 map__3698)
    a  (clojure.core/req! map__3698 :a)]
   a)

)

;;; If you want to protect user code from typos, you could detect overlap between required
;;; keys with and without bindings, or redundant required :keys!.  I suspect that those are
;;; cases of typographical errors, not sloppy thinking.

(comment

  (macroexpand '(let [{:keys! [a b & :c]} {:a 1 :b 2 :c 3}] (list a b)))

(let*
 [map__3705  {:a 1, :b 2, :c 3}
  map__3705  (if   (clojure.core/seq? map__3705)
   (if    (clojure.core/next map__3705)
    (clojure.lang.PersistentArrayMap/createAsIfByAssoc
     (clojure.core/to-array map__3705))
    (if     (clojure.core/seq map__3705)
     (clojure.core/first map__3705)
     clojure.lang.PersistentArrayMap/EMPTY))
   map__3705)
  a  (clojure.core/req! map__3705 :a)
  b  (clojure.core/req! map__3705 :b)
  ignore__3706  (clojure.core/req! map__3705 :c)]
 (list a b))



(macroexpand '(let [{:keys! [a b & :a]} {:a 1 :b 2 :c 3}] (list a b)))

;; redundant required keys a :a

(let*
 [map__3711  {:a 1, :b 2, :c 3}
  map__3711  (if   (clojure.core/seq? map__3711)
               (if    (clojure.core/next map__3711)
                 (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                  (clojure.core/to-array map__3711))
                 (if     (clojure.core/seq map__3711)
                   (clojure.core/first map__3711)
                   clojure.lang.PersistentArrayMap/EMPTY))
               map__3711)
  a  (clojure.core/req! map__3711 :a)
  b  (clojure.core/req! map__3711 :b)
  ignore__3712  (clojure.core/req! map__3711 :a)]
 (list a b))



)


;;; comments from Rich in Clojurians Slack:
;;; https://clojurians.slack.com/archives/C03S1KBA2/p1784129705894619?thread_ts=1784047669.360589&cid=C03S1KBA2


;;; Each of these pieces has a semantic. Those semantics combine. If you learn the semantics
;;; you don't need a ton of examples and speculations as to the mechanics. So:
;;;
;;; :as refers to the map being destructured. You need it when you are destructuring an
;;; argument, else there would be no way to talk about the actual argument.
;;;
;;; :or is a map you write to supply default values for keys (aka the defaults), used when
;;; those keys are sought but not present in the input map when a) initializing a binding b)
;;; constructing a :select result. You may specify values either via explicit keys or
;;; binding names. In the latter case the default is for the key associated with that
;;; binding name. If you haven't yet used that binding name in a binding elsewhere, it is
;;; not associated with any key and the entry is a no-op.
;;;
;;; :defaults binds a name to a map, created during destructuring, of keys to their default
;;; values. It is based on what you've said in :or per above. You would use this when you
;;; want those same default values for some purpose in the body, and don't want to restate
;;; them (DRY).
;;;
;;; :select binds a name to a map, created during destructuring, containing only those keys
;;; from the input map, or the defaults if not present in the input, that are used anywhere
;;; in the destructuring form (other than in :or), including in nested destructuring. Thus
;;; it is a (deep) subset of the input augmented by defaults.


;;; SEM: Rich's comments do not explicitly address my complaint about the need for "dead
;;; default optimization".




;;; looking at how the deep :select works

(comment
(macroexpand '(let [{{:keys [a d] :or {:d 4}} :aa  dd :dd :select sel :or {:dd 44}} dabc]
                sel))

(let*
 [default__3880  44
  map__3878  dabc
  map__3878  (if (clojure.core/seq? map__3878)
               (if (clojure.core/next map__3878)
                 (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                  (clojure.core/to-array map__3878))
                 (if (clojure.core/seq map__3878)
                   (clojure.core/first map__3878)
                   clojure.lang.PersistentArrayMap/EMPTY))
               map__3878)
  default__3885  4
  map__3883  (clojure.core/get map__3878 :aa)
  map__3883  (if (clojure.core/seq? map__3883)
               (if (clojure.core/next map__3883)
                 (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                  (clojure.core/to-array map__3883))
                 (if (clojure.core/seq map__3883)
                   (clojure.core/first map__3883)
                   clojure.lang.PersistentArrayMap/EMPTY))
               map__3883)
  a  (clojure.core/get map__3883 :a)
  d  (clojure.core/get map__3883 :d default__3885)
  select__3882  (clojure.core/when-let
                    [mm__6394__auto__ (clojure.core/merge
                                       (clojure.core/some-vals
                                        (clojure.core/select-keys {:d default__3885} #{:d :a}))
                                       map__3883
                                       (clojure.core/some-vals nil))]
                  (clojure.core/select-keys mm__6394__auto__ #{:d :a}))
  dd  (clojure.core/get map__3878 :dd default__3880)
  sel  (clojure.core/when-let
           [mm__6394__auto__ (clojure.core/merge
                              (clojure.core/some-vals
                               (clojure.core/select-keys {:dd default__3880} #{:aa :dd}))
                              map__3878
                              (clojure.core/some-vals {:aa select__3882}))]
         (clojure.core/select-keys mm__6394__auto__ #{:aa :dd}))]
 sel)


)



;;; new in 1.13
#_
(defn some-vals
  "Returns a map with only the non-nil values of map m. Returns nil if
  m has no non-nil vals."
  {:added "1.13"
   :static true}
  [m]
  (reduce-kv
   (fn [m k v] (if (some? v) (assoc m k v) m))
   nil m))

;;; SEM:  I suspect it would be faster to just eliminate the nils from the original
;;; (assuming they are not common).  But Rich would have already thought of this if it were
;;; a good idea!
(defn some-vals2
  "Returns a map with only the non-nil values of map m. Returns nil if
  m has no non-nil vals."
  {:added "1.13"
   :static true}
  [m]
  (reduce-kv (fn [m k v] (if (nil? v) (dissoc m k) m))
             m
             m))


;;; transient never worth it, always slower
(defn some-vals3
  "Returns a map with only the non-nil values of map m. Returns nil if
  m has no non-nil vals."
  {:added "1.13"
   :static true}
  [m]
  (persistent!
  (reduce-kv (fn [m k v] (if (nil? v) m (assoc! m k v)))
             (transient {})
             m)))

;;; not good
(defn some-vals4
  "Returns a map with only the non-nil values of map m. Returns nil if
  m has no non-nil vals."
  {:added "1.13"
   :static true}
  [m]
  (let [cnt (count m)
        threshold (- cnt 3)]
    (if (< cnt 3)
      (some-vals m)
      (reduce-kv (fn [m k v]
                   (cond (<= (count m) threshold) (reduced (some-vals m))
                         (nil? v) (dissoc m k)
                         :else m))
               m
               m))))


;;; need to test with kw maps as that's the common case
(def kw100 (mapv #(keyword (str "k" %)) (range 100)))

(def km (assoc (zipmap kw100 (range 100)) :a nil :b nil))

(def mostly-nil (assoc (zipmap kw100 (repeat 100 nil)) :a 1 :b 2))

;;; typical might count 10

(def k5 {:a 1 :b 2 :c 3 :d 4 :e nil})

(def k1 {:a 1 :b nil :c nil :d nil :e nil})

(def k7 {:a 1 :b 2 :c 3 :d 4 :e nil :f nil :g nil})
