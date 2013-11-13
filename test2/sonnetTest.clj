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
C.manner=SUDDENLY
C.time=PAST
C.type=FORGET
C.arg1=B
C.arg2=D
-- 3:
D@2.type=question
D@2.content=A
B.type=wh
A.time=PRESENT
A.type=COME_SCALARLY
C.order=EARLIER
C.anchor=D
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
    "Удивительный случай случился со мной"
    "An amazing thing happened to me today"))

