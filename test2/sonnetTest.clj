(ns SonnetTest
  (:use clojure.test parser))

(deftest sample
  (is (= (parse "abc def.") ["abc" "def" "."]))
  )

(print-log (parse "Удивительный случай случился со мной"))

(run-tests 'SonnetTest)
