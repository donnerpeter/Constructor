(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing true)

(ns allTests (:use clojure.test))

(require 'parsingStateTest 'sonnetTest)

(def start (System/currentTimeMillis))

(run-tests 'parsingStateTest #_'sonnetTest)

(println (str "Time: " (- (System/currentTimeMillis) start)))
