(ns parsingState
  (:require [mites :refer :all])
  )

(defrecord ParsingState [stack log mites enrich active happy?])
(defn empty-parsing-state [enrich happy?] (->ParsingState () "" () enrich #{} happy?))

(defn append-log [state newLog]
  (assoc state :log
    (str (:log state) newLog "\n")))
(defn print-log [state] (println (str "Log:" (:log state))))
(defn all-mites [state] (flatten (reverse (:mites state))))
(defn get-chart [state]
  (let [kotlin-cxt (fn [cxt] (cond
                               (= cxt :sem) (. cons4.constructions.sem instance$)
                               (= cxt :semSectionEnd) (. cons4.constructions.semSectionEnd instance$)
                               :else (. cons4.constructions.emptyCxt instance$)))
        active (filter #(contains? (:active state) %) (all-mites state))
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new java.util.LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (.cxt %)) (to-linked-map (.args %)) nil nil nil) active)]
    (new cons4.Chart kotlin-mites)))

(defn presentable [state]
  (let [present-mite (fn [mite] (str (if (contains? (:active state) mite) "*" "") (if ((:happy? state) mite) "" "!") mite))
        present-level (fn [level] (clojure.string/join " " (map present-mite level)))]
    (clojure.string/join "\n" (map #(str "  " (present-level %)) (:stack state)))))

(defn find-contradictors [state mite coll] (filter #(and (not= mite %) (mites-contradict mite %)) coll))
(defn contradictors [state mite] (find-contradictors state mite (all-mites state)))
(defn happy-contradictors [state mite] (filter #((:happy? state) %) (contradictors state mite)))
(defn unhappy-contradictors [state mite] (filter #(not ((:happy? state) %)) (contradictors state mite)))

(defn visible-mites [state] (flatten (:stack state)))

(defn suggest-active [state]
  (let [visible (visible-mites state)
        invisible (filter #(not (contains? visible %)) (all-mites state))
        all-unhappy (filter #(not ((:happy? state) %)) (all-mites state))
        all-happy (filter #((:happy? state) %) (all-mites state))
        weights (loop [map {}
                       rest-unhappy all-unhappy]
                  (if (empty? rest-unhappy) map
                    (let [contras (happy-contradictors state (first rest-unhappy))
                          weight-inc (if (empty? contras) 239 (/ 1 (count contras)))
                          new-map (loop [map map
                                         rest-contras contras]
                                    (if (empty? rest-contras) map
                                      (let [fst (first rest-contras)
                                            new-map (assoc map fst (max weight-inc (get map fst 0)))]
                                        (recur new-map (rest rest-contras)))
                                      )
                                    )]
                      (recur new-map (rest rest-unhappy))
                      )))
        suggested (loop
                    [result #{}
                         rest-happy (sort-by #(get weights % 1) all-happy)]
                    (if (empty? rest-happy) result
                      (recur
                        (if (empty? (find-contradictors state (first rest-happy) result)) (conj result (first rest-happy)) result)
                        (rest rest-happy)))
                    )
        ]
    suggested)
  )

(defn raw-add-mites [state mites]
  (let [stack (:stack state)
        state-mites (:mites state)
        new-state (assoc state
              :stack (cons (vec (concat (first stack) mites)) (next stack))
              :mites (cons (vec (concat (first state-mites) mites)) (next state-mites))
              )]
    (assoc new-state :active (suggest-active new-state))))

(defn add-mites-enriching [state mites]
  (loop [state state mites mites]
    (if (empty? mites)
      state
      (recur (raw-add-mites state mites) (flatten (map (:enrich state) mites))))))

(defn leave-previous-stack [left right]
  (and (has-hard left :head) (not (has-hard right :head))))

(defn do-merge-mites
  [state top remaining-stack]
  (if (empty? remaining-stack) []
    (concat (flatten (for [right top
                           left (first remaining-stack)]
                       (if-let [unified (unify left right)]
                         (let [leave (leave-previous-stack left right)
                               with-unified (raw-add-mites state [unified])
                               new-last-mites (first (:stack with-unified))
                               new-stack (cons new-last-mites (if leave remaining-stack (next remaining-stack)))]
                           [(assoc with-unified :stack new-stack)])
                         ())
                       )) (do-merge-mites state top (next remaining-stack)))))

(defn merge-mites [state]
  (let [[top & rest] (:stack state)
        allStates (do-merge-mites state top rest)]
    (if (empty? allStates) state (first allStates))))

(defn add-word [state mite]
  (let [newState (append-log (assoc state :stack (cons () (:stack state)) :mites (cons () (:mites state))) "\n---------------------------------")
        withAdded (add-mites-enriching newState [mite])
        finalState (merge-mites withAdded)]
    (append-log finalState (presentable finalState)))
  )
