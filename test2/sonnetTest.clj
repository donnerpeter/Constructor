(ns SonnetTest
  (:import [cons4 EnglishGenerator])
  (:use clojure.test parser parsingState))

(deftest sample
  (is (= (parse "abc def.") ["abc" "def" "."]))
  )

(let [state (parse "Удивительный случай случился со мной")
      chart (get-chart state)]
  (print-log state)
  (println (. chart presentable))
  (println (. (new EnglishGenerator) generate chart))
  )

(run-tests 'SonnetTest)
