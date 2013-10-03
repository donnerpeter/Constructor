(ns mites
  (:import [cons4 Variable])
  (:require clojure.string))

(defrecord Mite [cxt args])

(defmethod clojure.core/print-method mites.Mite [x writer]
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
