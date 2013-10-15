(ns parser
  (:require [mites :refer :all])
  (:require [parsingState :refer :all])
  (:require clojure.string)
  )

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
    (list (mite :phrase :head (v 0) :kind :verb) (mite :nom :head (v 0) :child (v :nom :light)))
    (sem (v 0) "time" time "type" type "arg1" (v :nom))))

(defn parse-word [word]
  (let [_vars (new cons4.Vars)
        v (fn ([key] (. _vars get key))
            ([key light] (. (. _vars get key) getLv)))
        var (v 0)]
    (case (clojure.string/lower-case word)
      "7" (concat (noun :nom var "7") (sem var "number" "true"))
      "8" (concat (noun :nom var "8") (sem var "number" "true"))
      "вдруг" (concat [(mite :phrase :kind :verb :head (v 0 :light))] (sem var "manner" "SUDDENLY"))
      "забыл" (concat (finiteVerb v "PAST" "FORGET") (arg v :comp "arg2"))
      "идет" (concat (finiteVerb v "PAST" "COME_SCALARLY") [(mite :order-adverb :head var)])
      "или" [(mite :seq :conj "or" :seqVar var :left (v 1) :right (v 2))]
      "мной" (pronoun :instr var "ME")
      "раньше" (concat [(mite :order-adverb :head (v 1 :light) :child var)] (sem (v 1) "order" "EARLIER"))
      "случай" (noun :nom var "THING")
      "случился" (concat (finiteVerb v "PAST" "HAPPEN") (arg v :sInstr "experiencer"))
      "со" (preposition v :sInstr :instr)
      "удивительный" (adj :nom (v 0 :light) "property" "AMAZING")
      "что" (concat [(mite :clauseType :child (v 2))] (pronoun :nom var "wh") [(mite :question :head (v 2) :questioned var)])
      "я" (pronoun :nom var "ME")
      "-" [(mite :questionVariants :variants var :dummyHead (v 1)) (mite :semSectionEnd :id var)]
      ":" (concat [(mite :semSectionEnd :id var) (mite :phrase :kind :verb :head (v 0 :light)) (mite :elaboration :head (v 0) :elaboration (v 1 :light) :first true)]
            (sem var "elaboration" (v 1)))
      "," [(mite :phrase :kind :verb :head (v 0 :light) :last true) (mite :sentence :head (v 1 :light))
           (mite :comp :child (v 2)) (mite :clauseType :head (v 0) :child (v 2 :light))
           (mite :semSectionEnd :id var)]
      '())))

(defn enrich [m]
  (let [cxt (.cxt m)]
    (cond
      (= cxt :word) (parse-word (:word (.args m)))
      (and (= cxt :phrase) (= (marg m :kind) :verb) (has-hard m :head))
        [(mite :elaboration :child (marg m :head))]
      :else ()
      ))
  )

(defn happy? [mite]
  (case (.cxt mite)
    (:nom :gen :dat :acc :instr :prep :sInstr :order-adverb :adverb) (has-hard mite :child :head)
    :phrase (has-hard mite :head)
    true
    ))

(defn parse-token [state token] (println token) (add-word state (mite :word :word token :id (. (new cons4.Vars) get 0))))

(defn parse [input]
  (let [tokenizer (new java.util.StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (empty-parsing-state enrich happy?) tokens)))
