(ns squaremo.mqtt.retain)

;; Similar to topic matching, but now we have a map of topic ->
;; messages, and must match topic patterns against them.

;; The simplest way is to keep a map of "full" topics to messages, and
;; scan it linearly for each pattern.

(defn pattern-matches?
  [pattern topic]
  (let [prefix (first pattern)]
    (case prefix
      ("#") true
      (nil) (empty? topic)
      ("+") (recur (rest pattern) (rest topic))
      (and (= prefix (first topic))
           (recur (rest pattern) (rest topic))))))

(defn empty-retainer
  [] {})

(defn retain
  [retainer topic value]
  (assoc retainer topic value))

(defn all-retained
  [retainer pattern]
  (set (for [r retainer
             :let [t (key r)]
             :when (pattern-matches? pattern t)]
         (val r))))
