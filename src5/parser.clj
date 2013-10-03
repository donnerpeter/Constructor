(ns parser
  (:require [mites :refer :all])
  (:require clojure.string)
  )

(defrecord ParsingState [stack log mites])

(defn append-log [state newLog] (assoc state :log (str (:log state) newLog "\n")))
(defn print-log [state] (println (str "Log:\n\n" (:log state))))
(defn all-mites [state] (flatten (:mites state)))
(defn get-chart [state]
  (let [kotlin-cxt (fn [cxt] (cond
                               (= cxt :sem) (. cons4.constructions.sem instance$)
                               (= cxt :semSectionEnd) (. cons4.constructions.semSectionEnd instance$)
                               :else (. cons4.constructions.emptyCxt instance$)))
        all (all-mites state)
        to-linked-map (fn [clj-map]
                        (reduce (fn [map key]
                                  (do
                                    (.put map (name key) (get clj-map key))
                                    map))
                          (new java.util.LinkedHashMap)
                          (keys clj-map))
                        )
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (:cxt %)) (to-linked-map (:args %)) nil nil nil) all)]
    (new cons4.Chart kotlin-mites)))

(defn presentable [state] (str (:stack state)))

(defn sem [var & pairs]
  (if (empty? pairs) ()
    (cons (mite :sem :frame var :attr (first pairs) :value (nth pairs 1)) (apply sem var (drop 2 pairs)))))
(defn adj [case var rel value] (cons (mite case :child var) (sem var rel value)))
(defn noun [case var type] (cons (mite case :child var) (sem var "type" type)))
(defn pronoun [case var type] (cons (mite case :child var) (sem var "type" type)))
(defn preposition [v prepCxt case] [(mite prepCxt :child (v 0)) (mite case :child (v 0 :light) :head (v 1))])
(defn arg [v case rel] (cons (mite case :head (v 0) :child (v case :light)) (sem (v 0) rel (v case))))
(defn finiteVerb [v time type]
  (concat
    (list (mite :phrase :head (v 0) :kind "verb") (mite :nom :head (v 0) :child (v :nom :light)))
    (sem (v 0) "time" time "type" type "arg1" (v :nom))))

(defn parse-word [word]
  (let [_vars (new cons4.Vars)
        v (fn ([key] (. _vars get key))
            ([key light] (. (. _vars get key) getLv)))
        var (v 0)]
    (case (clojure.string/lower-case word)
      "мной" (pronoun :instr var "ME")
      "случай" (noun :nom var "THING")
      "случился" (concat (finiteVerb v "PAST" "HAPPEN") (arg v :sInstr "experiencer"))
      "со" (preposition v :sInstr :instr)
      "удивительный" (adj :nom (v 0 :light) "property" "AMAZING")
      '())))

(defn enrich [mite]
  (case (:cxt mite)
    :word (parse-word (:word (:args mite)))
    '()))

(defn add-mites [state mites]
  (loop [state state mites mites]
    (if (empty? mites)
      state
      (let [stack (:stack state)
            state-mites (:mites state)
            newState (assoc state
                       :stack (cons (concat mites (first stack)) (next stack))
                       :mites (cons (concat mites (first state-mites)) (next state-mites))
                       )]
        (recur newState (flatten (map enrich mites))))
      )))

(defn execute-mite [mite against]
  (mapcat #(let [merged-args (if (= (:cxt %) (:cxt mite)) (merge-args (:args %) (:args mite)) nil)]
             (if (nil? merged-args) () (list (->Mite (:cxt mite) merged-args))))
    against))

(defn merge-mites [state]
  (let [[top & rest] (:stack state)
        merge-mite (fn [mite]
                     (if (empty? rest) () (map #(assoc state
                                                  :stack (cons (cons % top) (next rest))
                                                  :mites (cons (cons % (first (:mites state))) (next (:mites state))))
                                            (execute-mite mite (first rest)))))
        allStates (mapcat merge-mite top)]
    (if (empty? allStates) state (first allStates))))

(defn parse-token [state token]
  (let [mite (mite :word :word token :id (. (new cons4.Vars) get 0))
        newState (append-log (assoc state :stack (cons () (:stack state))) (str token " ---------------"))
        withAdded (add-mites newState [mite])
        finalState (merge-mites withAdded)]
    (append-log finalState (str "  " (presentable finalState))))
  )

(defn parse [input]
  (let [tokenizer (new java.util.StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (->ParsingState () "" ()) tokens)))
