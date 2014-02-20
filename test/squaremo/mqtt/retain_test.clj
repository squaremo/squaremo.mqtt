(ns squaremo.mqtt.retain_test
  (:use squaremo.mqtt.retain)
  (:require [clojure.test :as test]))

(test/deftest empty-matches-empty
  (test/is (pattern-matches? [] [])))

(test/deftest empty-doesnt-match-word
  (test/is (not (pattern-matches? [] ["foo"]))))

(test/deftest equal-matches
  (test/is (pattern-matches? ["foo" "baz"] ["foo" "baz"])))

(test/deftest unequal-doesnt-match
  (test/is (not (pattern-matches? ["foo" "bar"] ["bar" "foz"]))))

(test/deftest hash-matches-empty
  (test/is (pattern-matches? ["#"] [])))

(test/deftest hash-matches-topic
  (test/is (pattern-matches? ["#"] ["foo" "bar" "baz"])))

(test/deftest hash-matches-tail
  (test/is (pattern-matches? ["foo" "bar" "#"] ["foo" "bar" "baz"])))

(test/deftest hash-matches-empty-tail
  (test/is (pattern-matches? ["foo" "#"] ["foo"])))

(test/deftest plus-matches-word
  (test/is (pattern-matches? ["+"] ["foo"])))

(test/deftest plus-matches-in-middle
  (test/is (pattern-matches? ["foo" "+" "bar"] ["foo" "boo" "bar"])))

(test/deftest plus-not-get-out-of-jail-free
  (test/is (not (pattern-matches? ["foo" "+" "bar"] ["foo" "boo" "boz"]))))

;; ===

(test/deftest no-matches-in-empty
  (test/is (empty? (all-retained (empty-retainer) ["#"]))))

(test/deftest all-match-hash
  (let [r (-> (empty-retainer)
              (retain ["foo"] 1)
              (retain ["bar" "foo"] 2))]
    (test/is (= #{1 2} (all-retained r ["#"])))))

(test/deftest none-match
  (let [r (-> (empty-retainer)
              (retain ["foo"] 1)
              (retain ["bar"] 2)
              (retain ["foo" "bar"] 3))]
    (test/is (= #{} (all-retained r ["baz" "+"])))))

(test/deftest some-match
  (let [r (-> (empty-retainer)
              (retain ["foo"] 1)
              (retain ["bar"] 2)
              (retain ["foo" "bar"] 3))]
    (test/is (= #{3} (all-retained r ["+" "bar"])))))
