(ns miner.compile)


;; copied from Clojure/core private macro

(defmacro when-class [class-name & body]
  `(try
     (Class/forName ^String ~class-name)
     ~@body
     (catch ClassNotFoundException _#)))

(comment

(when-class "java.sql.Timestamp"
  (load "instant"))

)


;; by @clojuregrams on twitter
(defmacro locals []
 (into {}
  (map (juxt name identity))
  (keys &env)))



;; Robert Luo <lt@basecity.com> on Clojure mailing list

(defmacro opt-require
  "Optional requires rqr-clause and if it succeed do the body"
  {:style/indent 1}
  [rqr-clause & body]
  (when
      (try
        (require rqr-clause)
        true
        (catch Exception _
          false))
    `(do ~@body)))


;; SEM: but does this really work?  The require is done at compile time, but not necessarily
;; again at runtime if you happen to AOT.  Not sure what would happen.  Also, is the alias
;; known at runtime?  Might be safe to insert the (require ...) into the main body of the
;; `do`.  On the other hand, if the compiler figured it out, we don't need the alias at
;; runtime.  So I guess it does work.  But the runtime is not the same environment as the
;; compile time if AOT is involved so it can still fail later.

(comment

(opt-require [clojure.spec.alpha :as s]
 (s/def ::my-spec int?))

)
