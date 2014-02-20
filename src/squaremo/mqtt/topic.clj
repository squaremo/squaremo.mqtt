;; Subscription lookup

(ns squaremo.mqtt.topic
  (:refer-clojure :exclude [remove empty])
  (:use [clojure.set :only [union]]))

;; This structure represents a map of pattern->values, where patterns
;; are vectors of strings e.g., ["foo" "bar"] with no nil entries.

;; It differs from a straight-out map in that there may be more than
;; one value at a pattern; and patterns may have wildcards, "+"
;; meaning "any key" and "#" meaning "any remaining keys", when
;; looking up values. For this reason, it's implemented as a tree,
;; with each level matching one key (or wildcard) to a remainder, and
;; leaves being sets.

(defn empty "Return an empty map" [] {})

;; To make insertion and matching simpler, an empty topic is
;; represented as nil. Thus, (insert {} [] v) => {nil #{v}}

(defn insert
  "Associate value with the pattern in the tree."
  [tree pattern value]
  (update-in tree (conj pattern nil) (fnil conj #{}) value))

(defn matches
  "Return a set of all the values in tree with patterns matching
  topic"
  [tree topic]
  (loop [node tree
         topic topic
         backtrack '()
         result #{}]
    (cond
     ;; Dead end. We got here because a key didn't exist (or the whole
     ;; tree is empty).
     (nil? node)
     (if (empty? backtrack)
       result
       (recur (first backtrack) (second backtrack)
              (drop 2 backtrack) result))
     ;; No more keys to match, so look at topics that end here (keyed
     ;; by nil), and "all remaining" (keyed by "#").
     (empty? topic)
     (recur (node "#") topic backtrack
            (union result (node nil)))
     ;; Still topic and node to go; can backtrack to a "+" or "#" from
     ;; here.
     :else
     (recur (node (first topic)) (rest topic)
            (conj backtrack
                  (rest topic) (node "+")
                  [] (node "#")) result))))

;; There's no analogue to update-in, so we have to do this manually.
(defn remove
  "Dissociate value from pattern in the tree"
  [tree pattern value]

  (defn- zipup [child path]
    (loop [node child
           path path]
      (cond
       (empty? path)
       node
       (empty? node)
       (recur (dissoc (first path) (second path)) (drop 2 path))
       :else
       (recur (assoc (first path) (second path) node) (drop 2 path)))))

  (loop [node tree
         pattern pattern
         path '()]
    (cond
     (nil? node)
     tree
     (empty? pattern)
     (if-let [leaf (node nil)]
       (let [newleaf (disj leaf value)
             without (if (empty? newleaf) (dissoc node nil) node)]
         (zipup without path)))
     :else
     (recur (node (first pattern)) (rest pattern)
            (conj path (first pattern) node)))))
