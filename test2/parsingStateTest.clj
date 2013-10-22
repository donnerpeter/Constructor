(ns parsingStateTest
  (:import [cons4 EnglishGenerator Variable Vars])
  (:use clojure.test parsingState mites))

(def state (empty-parsing-state (fn [_] [])))

(def v (let [_vars (new Vars)]
         (fn
           ([key] (. _vars get key))
           ([key light] (. (. _vars get key) getLv)))))

(deftest headless-merge
  (let [mite1 (mite :nom :child (v 0 :light))
        mite2 (mite :nom :child (v 1))
        state (add-word state mite1)
        state (add-word state mite2)]
    (is (= [mite2 (unify mite1 mite2)] (visible-mites state)))
    ))
(deftest right-headed-merge
  (let [mite1 (mite :nom :child (v 0))
        mite2 (mite :nom :head (v 1))
        state (add-word state mite1)
        state (add-word state mite2)]
    (is (= [mite2 (unify mite1 mite2)] (visible-mites state)))
    ))
(deftest left-headed-merge
  (let [mite1 (mite :nom :head (v 0))
        mite2 (mite :nom :child (v 1))
        state (add-word state mite1)
        state (add-word state mite2)]
    (is (= [mite2 (unify mite1 mite2) mite1] (visible-mites state)))
    ))
