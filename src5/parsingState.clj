(ns parsingState
  (:require [mites :refer :all])
  )

(defrecord ParsingState [stack log mites enrich])

(defn append-log [state newLog]
  (assoc state :log
    (str (:log state) newLog "\n")))
(defn print-log [state] (println (str "Log:" (:log state))))
(defn all-mites [state] (reverse (flatten (:mites state))))
(defn get-chart [state]
  (let [kotlin-cxt (fn [cxt] (cond
                               (= cxt :sem) (. cons4.constructions.sem instance$)
                               (= cxt :semSectionEnd) (. cons4.constructions.semSectionEnd instance$)
                               :else (. cons4.constructions.emptyCxt instance$)))
        all (all-mites state)
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new java.util.LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (:cxt %)) (to-linked-map (:args %)) nil nil nil) all)]
    (new cons4.Chart kotlin-mites)))

(defn presentable [state] (clojure.string/join "\n" (map #(str "  " (reverse %)) (:stack state))))

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
        (recur newState (flatten (map (:enrich state) mites))))
      )))

(defn leave-previous-stack [left right]
  (and (has-hard left :head) (not (has-hard right :head))))

(defn merge-mites [state]
  (let [[top & rest] (:stack state)
        prev (if (empty? rest) () (first rest))
        allStates (flatten (for [right top
                        left prev]
                    (if-let [unified (unify left right)]
                      (let [leave (leave-previous-stack left right)]
                        [(assoc state
                         :stack (cons (cons unified top) (if leave rest (next rest)))
                         :mites (cons (cons unified (first (:mites state))) (next (:mites state))))])
                      ())
                    ))]

    (if (empty? allStates) state (first allStates))))

(defn add-word [state mite]
  (let [newState (append-log (assoc state :stack (cons () (:stack state))) "\n---------------------------------")
        withAdded (add-mites newState [mite])
        finalState (merge-mites withAdded)]
    (append-log finalState (presentable finalState)))
  )
