


;;;;; 01/14/17  18:31 by miner -- tweets

BrandonBloom: @technomancy @mpenet I also would want this to throw:

(let [[x] [1 2]] ...

Requiring you to write:

(let [[x & _] [1 2]] ...

     ---
     
BrandonBloom: @technomancy @mpenet Like I said, I definitely do want this to work in clj:

(if-let [[x :ok] (some-op-with-a-return-code a b c)]
  on-success
  on-failure)

---

My idea: if-opt

define a macro that's stricter about the "optional" tuple
[:ok val]
[:error error-data]

(let-opt [:k expr] then.. else...)
;; expr must evaluate to vector
;; first must be :k
;; if so, local "k" is bound and then.. executed
;; otherwise else...

Question: should it throw if expr is nil?  YES -- this is for paranoid anti-nil types
also throw if result wasn't exactly vector of count 2 -- we're trying to be strict.

Look up Jneen variant stuff -- Clojure/conj 2014

http://jneen.net/posts/2014-11-23-clojure-conj-variants-errata


https://stackoverflow.com/questions/10947636/idiomatic-way-to-represent-sum-type-either-a-b-in-clojure/40888981#40888981

[:left 123]
[:right "hello"]
To then destructure you would need to refer to core.match and use:

(match either
  [:left num-val] (do-something-to-num num-val)
  [:right str-val] (do-something-to-str str-val))


Eric Normand: sum type in Clojure
http://www.lispcast.com/idiomatic-way-to-represent-either
suggests a map with tags
I think I prefer the vector approach (or more specifically a map-entry).

--

But, still have to decide if vector must be count = 2, or if more data is allowed beyond
second 'value'.  I'm leaning towards exactly 2 so that the data is still self-contained
(probably a map).  Well, then... why not a distinguished key for the "tag" or 'type' field?
Yes, maybe...

Jneen said that was an anti-pattern, like lots of nils in a sparse table.  Explicit type
checks.  (SEM: Wrong axis of composition for methods, unused general keys)  Easy to mix up.

A hashmap models AND (product types)

Model OR (sum types) with [:tag val] independently.  Core.match match will do the right destructing.

Really have to work out some examples and decide what composes best and gives good
performance, etc.


Jneen: "Variants are tagged data"  [:tag data]

My thought: well then `#tag data`, tag-reader could do that too as an alternative notation.
Not sure if that buys you anything.  Is it easier to type or understand? Probably not.



----------------------------------------------------------------------
SEM: core.match already does that sort of binding that I'm suggesting.

(if-opt [:key local expr]
    ;; key checked against expr result as second (peek?) of vector
    ;; if match, local bound to result
    (then...)
    (else...))

(if-opt [local expr] (then...) (else...))
;; same as if key :ok was specified
;; expr must return vector of [val key] pair

(if-opt [:key expr] (then...) (else...))
;; :key matches first result expr
;; local "key" bound within then...

(case-opt expr
          :key  (then...)
          :k2  (then...)
          :k3  (then...)
          default...)



[v 0] ==> success key
[v 1] ==> value

OR the other way around

Now I think I want the key first.  key/value pair makes sense

or should it be a map-entry? rather than a vector? maybe more efficient but not so easy to
type literal

Basically, a map-entry is a simple vector pair so that a map-entry would always work.
clojure.lang.IMapEntry could work, or leave it more general as
clojure.lang.IPersistentVector????

