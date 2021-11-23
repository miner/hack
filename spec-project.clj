;; Unofficial project.clj, just for SEM testing
;;
;;  Check official Clojure repo to get right version that Clojure is using.
;;  Run `lein install` to install this so that Clojure will work.
;;  Note: spec is not published to maven central even though Clojure uses it internally.
;;  Maybe someday I should use the "new" deps.edn approach to simplify my installation.

(defproject org.clojure/spec.alpha "0.3.214"
  :description "Unofficial spec alpha"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.0-alpha2"]
                 [org.clojure/test.check "1.1.0"]]
  ;; try not to overwrite original pom
  :pom-location "target/"
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"])
