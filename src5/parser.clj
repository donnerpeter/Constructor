(ns parser
  (:import [java.util StringTokenizer])
  (:require clojure.string)
  )

(defrecord ParsingState [stack])
(defrecord Mite [cxt args])

(defmethod clojure.core/print-method parser.Mite [x writer]
  (let [args (:args x)
        seq-args (seq args)
        pair-strings (map (fn [[key value]] (str (name key) "=" (pr-str value))) seq-args)
        arg-string (clojure.string/join "," pair-strings)
         ]
    (.write writer (str (name (:cxt x)) "(" arg-string ")"))))

(defn mite [cxt & args] (->Mite cxt (apply hash-map args)))
(defn sem [v attr value] (list (mite :sem :frame v :attr attr :value value)))
(defn adj [case v rel value] (cons (mite case :noun v) (sem v rel value)))

(defn parse-word [word]
  (case (clojure.string/lower-case word)
    "удивительный" (adj :nom nil "property" "AMAZING")
    '()))

(defn enrich [mite]
  (case (:cxt mite)
    :word (parse-word (:word (:args mite)))
    '()))

(defn add-mites [state mites]
  (if (empty? mites)
    state
    (let [[head & tail] (:stack state)
          newHead (concat mites head)
          newStack (cons newHead tail)
          newState (assoc state :stack newStack)]
      (add-mites newState (flatten (map enrich mites))))
    ))

(defn parse-token [state token]
  (let [mite (mite :word :word token)
        newState (assoc state :stack (cons () (:stack state)))]
    (add-mites newState (list mite)))
  )

(defn parse [input]
  (let [tokenizer (new StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (->ParsingState '()) tokens)))