(ns parser
  (:import [java.util StringTokenizer])
  (:require clojure.string)
  )

(defrecord ParsingState [mites])
(defrecord Mite [cxt args])

(defmethod clojure.core/print-method parser.Mite [x writer]
  (let [args (:args x)
        seq-args (seq args)
        pair-strings (map (fn [[key value]] (str (name key) "=" (pr-str value))) seq-args)
        arg-string (clojure.string/join ", " pair-strings)
         ]
    (.write writer (str (name (:cxt x)) "(" arg-string ")"))))

(defn parse-token [state token]
  (assoc state :mites (cons (->Mite :word {:word token}) (:mites state)))
  )

(defn parse [input]
  (let [tokenizer (new StringTokenizer input " .,:?!-" true)
        tokens (filter (fn [t] (not= t " ")) (enumeration-seq tokenizer))]
    (reduce parse-token (->ParsingState '()) tokens)))