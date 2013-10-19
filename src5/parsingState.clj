(ns parsingState
  (:require [mites :refer :all])
  (:use clojure.data.priority-map)
  )

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defrecord ParsingState [stack log mites enrich active happy? contradictor-cache])
(defn empty-parsing-state [enrich happy?] (->ParsingState () "" () enrich #{} happy? {}))
(defrecord StackLevel [link-up mites])

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
        active (filter #(in? (:active state) %) (all-mites state))
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new java.util.LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (.cxt %)) (to-linked-map (.args %)) nil nil nil) active)]
    (new cons4.Chart kotlin-mites)))

(defn visible-mites [state] (flatten (:stack state)))

(defn presentable [state]
  (let [visible (visible-mites state)
        additional (filter #(and (in? (:active state) %) (not ((:happy? state) %)) (not (in? visible %))) (all-mites state))
        present-mite (fn [mite] (str (if (in? (:active state) mite) "*" "") (if ((:happy? state) mite) "" "!") mite))
        present-level (fn [level] (str (if (:link-up level) "/ " "| ") (clojure.string/join " " (map present-mite (:mites level)))))
        additional-str (if (empty? additional) "" (str "\n    unhappy: " (clojure.string/join " " (map present-mite additional))))]
    (str (clojure.string/join "\n" (map #(str "  " (present-level %)) (:stack state))) additional-str)))

(defn find-contradictors [state mite coll] (filter #(and (not= mite %) (mites-contradict mite %)) coll))
(defn contradictors [state mite] (get (:contradictor-cache state) mite))
(defn happy-contradictors [state mite] (filter #((:happy? state) %) (contradictors state mite)))
(defn unhappy-contradictors [state mite] (filter #(not ((:happy? state) %)) (contradictors state mite)))

(defrecord ActiveChange [chosen remaining uncovered])

(defn is-uncovered? [mite state chosen-map]
  (and
    (every? #(= false (get chosen-map %)) (happy-contradictors state mite))
    (or
      (= false (get chosen-map mite))
      (not ((:happy? state) mite)))
    ))

(defn update-uncovered [expelled-coll state ac]
  (let [suspicious (clojure.set/union (set (mapcat #(contradictors state %) expelled-coll)) expelled-coll)
        fresh-uncovered (filter #(is-uncovered? % state (:chosen ac)) suspicious)]
    (assoc ac :uncovered (clojure.set/union (:uncovered ac) fresh-uncovered))))

(defn is-complete-change? [ac] (empty? (:remaining ac)))
(defn fork-change [state ac]
  (let [mite (first (:remaining ac))
        rest-remaining (rest (:remaining ac))
        taken (let [to-expel (filter #(mites-contradict mite %) rest-remaining)
                    expelled-map (reduce #(assoc %1 %2 false) (:chosen ac) to-expel)
                    ]
                (update-uncovered to-expel state (assoc ac :chosen (assoc expelled-map mite true) :remaining (filter #(not (in? to-expel %)) rest-remaining))))
        omitted (update-uncovered [mite] state (assoc ac :chosen (assoc (:chosen ac) mite false) :remaining rest-remaining))]
    [taken omitted]))

(defn apply-change [ac state]
  (let [all (all-mites state)
        uncovered (:uncovered ac)
        new-active (filter
                     #(or (in? uncovered %) (= true (get (:chosen ac) %))) all)]
    new-active))

(defn build-contradictor-cache [state]
  (let [all (all-mites state)]
    (zipmap all (map #(find-contradictors state % all) all))))

(defn suggest-active [state]
  (let [state (assoc state :contradictor-cache (build-contradictor-cache state))
        visible (visible-mites state)
        invisible (set (filter #(not (in? visible %)) (all-mites state)))
        all-unhappy (filter #(not ((:happy? state) %)) (all-mites state))
        all-happy (filter #((:happy? state) %) (all-mites state))
        change-weight (fn [ac]
                        (let [uncovered (:uncovered ac)
                              invisible-uncovered (count (filter #(in? invisible %) uncovered))]
                          [invisible-uncovered (- (count uncovered) invisible-uncovered)]))
        initial-change (->ActiveChange {} all-happy #{})
        ]
    (loop [queue (priority-map initial-change (change-weight initial-change))]
      (let [[next-ac & weight] (peek queue)
            queue (pop queue)]
        (if (is-complete-change? next-ac)
          (apply-change next-ac state)
          (let [forked (fork-change state next-ac)
                queue (reduce #(assoc %1 %2 (change-weight %2)) queue forked)]
            (recur queue))))
      ))
  )

(defn raw-add-mites [state mites]
  (let [stack (:stack state)
        state-mites (:mites state)
        stack-top (first stack)
        new-state (assoc state
              :stack (cons (assoc stack-top :mites (vec (concat (:mites stack-top) mites))) (next stack))
              :mites (cons (vec (concat (first state-mites) mites)) (next state-mites))
              )
        new-active (suggest-active new-state)]
    (assoc new-state :active new-active)))

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
    (let [all-unified (flatten (for [right top
                                     left (:mites (first remaining-stack))]
                                 (if-let [unified (unify left right)] [unified] ())
                                 ))
          mites-leaving-stack (filter #(leave-previous-stack (.src1 %) (.src2 %)) all-unified)
          mites-eating-stack (filter #(not (in? mites-leaving-stack %)) all-unified)
          add-merged-mites (fn [merged-mites leave-stack]
                             (if (empty? merged-mites) nil
                               (let [with-unified (raw-add-mites state merged-mites)
                                     new-last-mites (:mites (first (:stack with-unified)))
                                     new-stack (cons (->StackLevel leave-stack new-last-mites) (if leave-stack remaining-stack (next remaining-stack)))]
                                 [(assoc with-unified :stack new-stack)])))
           all-merges (filter #(not= % nil) [(add-merged-mites mites-leaving-stack true) (add-merged-mites mites-eating-stack false)])]
      (concat (flatten all-merges)
        (if (:link-up (first remaining-stack)) (do-merge-mites state top (next remaining-stack)) [])))))

(defn merge-mites [state]
  (let [[top & rest] (:stack state)]
    (if (:link-up top)
      state
      (let [allStates (do-merge-mites state (:mites top) rest)]
        (if (empty? allStates) state (merge-mites (first allStates)))))))

;(Thread/sleep 10000)

(defn add-word [state mite]
  (let [newState (append-log (assoc state :stack (cons (->StackLevel false []) (:stack state)) :mites (cons () (:mites state))) "\n---------------------------------")
        withAdded (add-mites-enriching newState [mite])
        finalState (merge-mites withAdded)]
    (append-log finalState (presentable finalState)))
  )
