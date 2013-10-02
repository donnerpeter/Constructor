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
  (let [all-keys (set (concat (keys args1) (keys args2)))
        merge-one (fn [key val1 val2]
                    (cond
                      (= val1 nil) val2
                      (= val2 nil) val1
                      (= val1 val2) val1
                      (and (instance? Variable val1) (instance? Variable val2) (not (and (.getHard val1) (.getHard val2)))) (. Variable/object$ mergeVars val1 val2)
                      :else nil
                      )
                    )]
    (reduce
      #(let [merged (merge-one %2 (%2 args1) (%2 args2))]
               (if (= nil merged) nil
                 (assoc %1 %2 merged)))
      {} all-keys)
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

(defn execute-mite [mite against]
  (mapcat #(let [merged-args (if (= (:cxt %) (:cxt mite)) (merge-args (:args %) (:args mite)) nil)]
             (if (nil? merged-args) () (list (->Mite (:cxt mite) merged-args))))
    against))

(defn merge-mites [state]
  (let [[top & rest] (:stack state)
        merge-mite (fn [mite]
                     (if (empty? rest) () (map #(->ParsingState (cons (cons % top) (next rest))) (execute-mite mite (first rest)))))
        allStates (mapcat merge-mite top)]
    (if (empty? allStates) state (first allStates))))

(defn parse-token [state token]
  (let [mite (mite :word :word token)
        newState (assoc state :stack (cons () (:stack state)))
        withAdded (add-mites newState (list mite))]
    (merge-mites withAdded))
  )

(defn parse [input]
  (let [tokenizer (new StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (->ParsingState '()) tokens)))
