(ns sonnetTest
  (:import [cons4 EnglishGenerator Variable Tokens])
  (:use clojure.test parser parsingState))

(defn doParseTest [input expected]
  (. (Variable/object$) resetCounter)
  (. (Tokens/object$) resetCounter)
  (let [state (parse input)
        chart (get-chart state)
        actual (. chart presentable)]
    (if (not= actual expected) (print-log state))
    (is (= (str (.trim expected) "\n") (str actual)))
    ))

(defn doTranslateTest [input expected]
  (. (Variable/object$) resetCounter)
  (. (Tokens/object$) resetCounter)
  (let [state (parse input)
        chart (try (get-chart state) (catch Exception e (print-log state) (throw e)))
        actual (.generate (new EnglishGenerator) chart)]
    (if (not= actual expected) (do (println (.presentable chart)) (print-log state)))
    (is (= expected actual))
    )
  )

(deftest parse1
  (doParseTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8" "
A.property=AMAZING
A.type=THING
B.time=PAST
B.type=HAPPEN
B.arg1=A
B.experiencer=C
C.type=ME
-- 2:
B@1.elaboration=A
B.type=ME
A.manner=SUDDENLY
A.time=PAST
A.type=FORGET
A.arg1=B
A.arg2=C
-- 3:
A.type=wh
B.time=PRESENT
B.type=COME_SCALARLY
B.arg1=A
B.order=EARLIER
C@2.type=question
C@2.content=B
-- 4:
A@3.variants=A
B.type=7
B.number=true
A.member=B
C.type=8
C.number=true
A.conj=or
A.member=C
")
  )

(deftest translate1
  (doTranslateTest
    "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8"
    "An amazing thing happened to me today"))

