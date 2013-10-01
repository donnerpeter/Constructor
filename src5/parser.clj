(ns parser
  (:import [java.util StringTokenizer]
           [cons4 Variable])
  (:require clojure.string)
  )

(defrecord ParsingState [stack])
(defrecord Mite [cxt args])

(defmethod clojure.core/print-method parser.Mite [x writer]
  (let [args (:args x)
        seq-args (seq args)
        pair-strings (map (fn [[key value]] (str (name key) "=" value)) seq-args)
        arg-string (clojure.string/join "," pair-strings)
         ]
    (.write writer (str (name (:cxt x)) "(" arg-string ")"))))

(defn merge-args [args1, args2]
  (let [all-keys (hash-set (conj (keys args1) (keys args2)))
        merge-one (fn [key val1 val2]
                    (cond 
                      (= val1 nil) val2
                      (= val2 nil) val1
                      (= val1 val2) val1
                      (and (instance? val1 Variable) (instance? val2 Variable) (not (and (.isHard val1) (.isHard val2)))) (. Variable/object$ mergeVars val1 val2)
                      :else nil
                      )
                    )]
    (reduce #(let [merged (merge-one %2 (%2 args1) (%2 args2))] (if (= nil merged) nil (assoc %1 %2 merged))) {} all-keys) 
    )
  )

(defn mite [cxt & args] (->Mite cxt (apply hash-map args)))
(defn sem [v attr value] (list (mite :sem :frame v :attr attr :value value)))
(defn adj [case v rel value] (cons (mite case :noun v) (sem v rel value)))
(defn noun [case v type] (cons (mite case :noun v) (sem v "type" type)))

(defn parse-word [word]
  (let [v (new cons4.Vars)
        vh (fn [key] (. v get key))
        vl (fn [key] (. (. v get key) getLv))
        var (vh 0)]
    (case (clojure.string/lower-case word)
    "случай" (noun :nom var "THING")
    "удивительный" (adj :nom (vl 0) "property" "AMAZING")
    '())))

(defn enrich [mite]
  (case (:cxt mite)
    :word (parse-word (:word (:args mite)))
    '()))

(defn add-mites [state mites]
  (loop [state state mites mites]
    (if (empty? mites)
      state
      (let [[head & tail] (:stack state)
            newHead (concat mites head)
            newStack (cons newHead tail)
            newState (assoc state :stack newStack)]
        (recur newState (flatten (map enrich mites))))
      )))

(defn parse-token [state token]
  (let [mite (mite :word :word token)
        newState (assoc state :stack (cons () (:stack state)))]
    (add-mites newState (list mite)))
  )

(defn parse [input]
  (let [tokenizer (new StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (->ParsingState '()) tokens)))
