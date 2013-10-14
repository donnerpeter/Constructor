(ns mites
  (:import [cons4 Variable])
  (:require clojure.string clojure.set))

(deftype Mite [cxt args src1 src2]
  Object (toString [x] (let [args (.args x)
                                 seq-args (seq args)
                                 pair-strings (map (fn [[key value]] (str (name key) "=" value)) seq-args)
                                 arg-string (clojure.string/join "," pair-strings)
                                 ]
                          (str (name (.cxt x)) "(" arg-string ")"))))

(defmethod clojure.core/print-method mites.Mite [x writer] (.write writer (str x)))

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
      #(if (= nil %1) nil
         (let [merged (merge-one %2 (%2 args1) (%2 args2))]
           (if (= nil merged) nil
           (assoc %1 %2 merged))))
      {} all-keys)
    )
  )

(defn- primaries [^Mite mite]
  (if (.src1 mite) (concat (primaries (.src1 mite)) (primaries (.src2 mite))) [mite]))

(defn mites-contradict [^Mite mite1 ^Mite mite2]
  (not (empty? (clojure.set/intersection (set (primaries mite1)) (set (primaries mite2))))))

(defn mite [cxt & args] (->Mite cxt (apply hash-map args) nil nil))
(defn marg [mite arg-name] (arg-name (.args mite)))

(defn may-unify [left right]
  (cond
    (marg right :first) false
    (marg left :last) false
    :else true))

(defn unify [left right]
  (when-let [merged-args (if (and (= (.cxt left) (.cxt right)) (may-unify left right)) (merge-args (.args left) (.args right)) nil)]
    (->Mite (.cxt left) merged-args left right)))

(defn has-var [mite & arg-names]
  (every? #(when-let [var (marg mite %)] (and (instance? Variable var))) arg-names))
(defn has-hard [mite & arg-names]
  (every? #(and (has-var mite %) (.booleanValue (.getHard (marg mite %)))) arg-names))
