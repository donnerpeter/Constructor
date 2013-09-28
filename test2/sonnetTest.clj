(ns SonnetTest
  (:use clojure.test parser))

(deftest sample
  (is (= (parse "abc def.") ["abc" "def" "."]))
  )

(println (parse "Удивительный случай случился со мной"))

(run-tests 'SonnetTest)