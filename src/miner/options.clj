;; SEM: decided that this was a bad idea.  Unless it was built-in, it's too confusing.

;; Discussion on Slack regarding preference for kw-args vs. single map for "options"
;; Kw-args is convenient for user-interaction with high-level APIs, but the single-map is
;; better for composing functions.

;; I thought maybe you could have both.  Treat :opts (or :whatever) special as a map of
;; defaults.  Regular :kw args get preference.  Traditional kw defaults would have to be
;; handled specially so that they take lowest precedence.


(defn kwargs-classic [& {:keys [x y z] :or {x 10 y 11 z 12} :as opts}] {:x x :y y :z z})

(defn kwargs [& {:keys [x y z] :as opts}] (merge {:x 11 :y 22 :z 33} opts))


(defn kwopts1 [& {:keys [x y z opts] :as kwopts}] (merge {:x 11 :y 22 :z 33}
                                                        (:opts kwopts)
                                                        (dissoc kwopts :opts)))

;; could be made into a macro 
(defn kwopts [& {:keys [x y z opts] :as kwopts}]
  (let [defaults {:x 11 :y 22 :z 33}
        x (or x (:x opts) (:x defaults))
        y (or y (:y opts) (:y defaults))
        z (or z (:z opts) (:z defaults))]
    {:x x :y y :z z}))


(defmacro kw-defaults [defaults] ...)
