(ns SonnetTest
  (:import [cons4 EnglishGenerator])
  (:use clojure.test parser parsingState))

(defn doParseTest [input expected]
  (. (cons4.Variable/object$) resetCounter)
  (. (cons4.Tokens/object$) resetCounter)
  (let [state (parse input)
        chart (get-chart state)
        actual (. chart presentable)]
    (if (not= actual expected) (print-log state))
    (is (= (str (.trim expected) "\n") (str actual)))
    ))

(defn doTranslateTest [input expected]
  (. (cons4.Variable/object$) resetCounter)
  (. (cons4.Tokens/object$) resetCounter)
  (let [state (parse input)
        chart (get-chart state)
        actual (.generate (new cons4.EnglishGenerator) chart)]
    (if (not= actual expected) (do (println (.presentable chart)) (print-log state)))
    (is (= expected actual))
    )
  )

(deftest parse1
  (doParseTest "Удивительный случай случился со мной" "
A.property=AMAZING
A.type=THING
B.experiencer=C
B.arg1=A
B.type=HAPPEN
B.time=PAST
C.type=ME
-- 2:
A.type=ME
B.type=FORGET
B.time=PAST
B.arg1=A
A.gender=masc
A.person=3
B.manner=SUDDENLY
B@1.elaboration=B
-- 3:
A.type=wh
B@2.arg2=B
C.time=PRESENT
C.arg1=A
A.person=3
B.content=C
B.type=question
B.questioned=A
C.type=COME_SCALARLY
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

(run-tests 'SonnetTest)
