(ns squaremo.mqtt.topic_test
  (:require [squaremo.mqtt.topic :as subs]
            [clojure.test :as test]))

(test/deftest empty-is-empty
  (test/is (empty? (subs/empty))))

(test/deftest empty-matches-nothing
  (test/is (empty? (subs/matches (subs/empty) []))))

(test/deftest match-inserted
  (test/is (= #{1} (let [topic ["foo" "bar"]
                         withsub (subs/insert (subs/empty) topic 1)]
                     (subs/matches withsub topic)))))

(test/deftest removed-is-empty
  (test/is (let [topic ["foo" "bar"]
                 withsub (subs/insert (subs/empty) topic 1)
                 withoutsub (subs/remove withsub topic 1)]
             (empty? withoutsub))))

(test/deftest no-matches-once-removed
  (test/is (empty? (let [topic1 ["foo" "bar"]
                         topic2 ["foo" "bar" "baz"]
                         withsub1 (subs/insert (subs/empty) topic1 1)
                         withsub2 (subs/insert withsub1 topic2 2)
                         without1 (subs/remove withsub2 topic1 1)]
                     (subs/matches without1 topic1)))))

(test/deftest matches-unremoved
  (test/is (= #{2} (let [topic1 ["foo" "bar"]
                         topic2 ["foo" "bar" "baz"]
                         withsub1 (subs/insert (subs/empty) topic1 1)
                         withsub2 (subs/insert withsub1 topic2 2)
                         without1 (subs/remove withsub2 topic1 1)]
                     (subs/matches without1 topic2)))))

(test/deftest match-wildcard
  (test/is (= #{1} (let [withwildcard (subs/insert (subs/empty) ["+"] 1)]
                     (subs/matches withwildcard ["foo"])))))

(test/deftest match-wildcard-and-equal
  (test/is (= #{1 2} (let [withtopic (subs/insert (subs/empty) ["foo"] 1)
                           withwildcard (subs/insert withtopic ["+"] 2)]
                       (subs/matches withwildcard ["foo"])))))

(test/deftest no-match-wildcard
  (test/is (= #{} (let [withwildcard
                        (subs/insert (subs/empty) ["foo" "+" "bar"] 1)]
                    (subs/matches withwildcard ["foo" "GARP" "boo"])))))

(test/deftest match-remainder
  (test/is (= #{1} (let [withremainder
                         (subs/insert (subs/empty) ["foo" "#"] 1)]
                     (subs/matches withremainder ["foo" "bar" "baz"])))))

(test/deftest match-remainder-internal
  (test/is (= #{1} (let [withtopic
                         (subs/insert (subs/empty) ["foo" "bar"] 2)
                         withremainder
                         (subs/insert withtopic ["foo" "#"] 1)]
                     (subs/matches withremainder ["foo" "bar" "boo"])))))
