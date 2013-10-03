(ns parsingState
  (:require [mites :refer :all])
  )

(defrecord ParsingState [stack log mites enrich])

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
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new java.util.LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (:cxt %)) (to-linked-map (:args %)) nil nil nil) all)]
    (new cons4.Chart kotlin-mites)))

(defn presentable [state] (str (:stack state)))

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
