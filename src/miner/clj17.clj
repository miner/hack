(ns miner.clj17
  (:import clojure.core.Eduction))

;; I didn't like the arg list for eduction because it takes xform* before a single
;; collection arg.  Normal variadic functions would put the singe arg first.  But they want
;; the collection arg last for aesthetic reasons apparently.
;;
;; ----------------------------------------------------------------------
;; On Apr 1, 2015, at 11:16 AM, Alex Miller <al...@puredanger.com> wrote: 
;;  
;;  - eduction now takes multiple transformations, not just one, and composes them. This is
;;  designed for mechanical rewriting (hello tool developers!!) of ->> chains like this:
;;  
;;  (->> s (interpose 5) (partition-all 2)) 
;;  
;;  to this: 
;;  
;;  (->> s (eduction (interpose 5) (partition-all 2))) 
;;
;; ----------------------------------------------------------------------
;; On Apr 3, 2015, at 11:08 AM, Steve Miner <steveminer@gmail.com> wrote:
;; 
;; Maybe it’s just me, but the eduction argument order just looks strange to me.  For a
;; variadic function, I would have expected the single collection to come first, then any
;; number of xforms.  There must be a better reason than mechanical rewriting.  Wouldn't a
;; macro make more sense?  [See educe->> below]
;;
;; So the rewrite could be just educe->> for ->>, without having to wrap the xforms at all.
;;   (educe->> s (interpose 5) (partition-all 2))
;;
;;
;; ----------------------------------------------------------------------
;; On Apr 3, 2015, at 11:37 AM, Alex Miller <alex@puredanger.com> wrote:
;; 
;; I understand your point and there are several competing comparisons here. Generally only
;; collection functions take the coll first. eduction is similar to sequence (and into and
;; reduce and transduce) in taking it last (but differs in taking multiple xforms). The
;; multiple xforms are similar to ->> though which works left to right and puts the source
;; first. I think I'd rather emphasize it's similarity to sequence than its similarity to ->>.


;; replace ->> usage with an eduction using educe->>
(defmacro educe->> [coll & xfs]
  `(->Eduction (comp ~@xfs) ~coll))

;; My unpublished proposed unrolling arities for eduction keeping the variadic version without
;; forcing me to pay for it.  Mostly pedantic but still it's the right thing to do.
(defn MY-eduction
  "Returns a reducible/iterable application of the transducers
  to the items in coll. Transducers are applied in order as if
  combined with comp. Note that these applications will be
  performed every time reduce/iterator is called."
  {:arglists '([xform* coll])
   :added "1.7"}
  ([xf coll] (Eduction. xf coll))
  ([xf xf2 coll] (Eduction. (comp xf xf2) coll))
  ([xf xf2 xf3 & xforms]
   (Eduction. (apply comp xf xf2 xf3 (butlast xforms)) (last xforms))))

