(ns parsingStateTest
  (:import [cons4 EnglishGenerator Variable Vars])
  (:use clojure.test parsingState mites))

(def state (empty-parsing-state (fn [_] [])))

(def v (let [_vars (new Vars)]
         (fn
           ([key] (. _vars get key))
           ([key light] (. (. _vars get key) getLv)))))

(defn add-mites [state mites]
  (let [node  (init-node (first mites) (fn [mite] (if (= mite (first mites)) (rest mites) [])))
         tree (new-tree node nil nil)]
    (add-tree state tree)))

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
    (is (= [mite2 mite1 (unify mite1 mite2)] (visible-mites state)))
    ))
(deftest right&left
  (let [mite1 (mite :nom :child (v 0))
        mites2 [(mite :nom :head (v 1)) (mite :acc :head (v 1))]
        mite3 (mite :acc :child (v 2))
        state (add-mites state [mite1])
        state (add-mites state mites2)
        state (add-mites state [mite3])
        ]
    (is (= (concat [mite3] mites2 [(unify mite1 (mites2 0)) (unify (mites2 1) mite3)]) (visible-mites state)))
    ))
(deftest left&left-same-head
  (let [mites1 [(mite :nom :head (v 0)) (mite :acc :head (v 0))]
        mite2 (mite :nom :child (v 1))
        mite3 (mite :acc :child (v 2))
        state (add-mites state mites1)
        state (add-mites state [mite2])
        state (add-mites state [mite3])
        ]
    (is (= (concat [mite3] mites1 [(unify (mites1 0) mite2) (unify (mites1 1) mite3)]) (visible-mites state)))
    ))
(deftest left&left-chaining
  (let [mite1 (mite :nom :head (v 0))
        mites2 [(mite :nom :child (v 1)) (mite :acc :head (v 1))]
        mite3 (mite :acc :child (v 2))
        state (add-mites state [mite1])
        state (add-mites state mites2)
        state (add-mites state [mite3])
        ]
    (is (= (concat [mite3] mites2 [(unify (mites2 1) mite3) mite1 (unify mite1 (mites2 0))]) (visible-mites state)))
    ))

(deftest spine-should-not-contradict-itself
  (let [mite1 (mite :nom :child (v 0 :light))
        mite2 (mite :nom :child (v 1))
        mite3 (mite :nom :head (v 2))
        state (add-mites state [mite1])
        state (add-mites state [mite2])
        state (add-mites state [mite3])
        all (all-mites state)
        ]
    (is (in? all (unify (unify mite1 mite2) mite3)))
    (is (in? all (unify mite1 mite2)))
    ))
