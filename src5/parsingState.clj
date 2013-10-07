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

(defn raw-add-mites [state mites]
  (let [stack (:stack state)
        state-mites (:mites state)]
    (assoc state
      :stack (cons (vec (concat (first stack) mites)) (next stack))
      :mites (cons (vec (concat (first state-mites) mites)) (next state-mites))
      :active (clojure.set/union (:active state) (set mites))
      )))

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
