(ns parsingState
  (:import (cons4 Chart)
           (cons4.constructions sem semSectionEnd emptyCxt)
           (java.util LinkedHashMap))
  (:require [mites :refer :all])
  (:use clojure.data.priority-map)
  )

(defn in? [seq elm] (some #(= elm %) seq))

(defrecord ParsingState [trees log enrich])
(defn empty-parsing-state [enrich] (->ParsingState () "" enrich))

(defrecord Tree [root mites active contradictor-map left-child right-child])
(defn empty-tree [root] (->Tree root [] #{} {} nil nil))

(defn append-log [state newLog]
  (assoc state :log
    (str (.log state) newLog "\n")))
(defn print-log [state] (println (str "Log:" (:log state))))
(defn all-mites [state] (flatten (reverse (map #(:mites %) (:trees state)))))
(defn get-chart [state]
  (let [kotlin-cxt (fn [cxt] (cond
                               (= cxt :sem) (. sem instance$)
                               (= cxt :semSectionEnd) (. semSectionEnd instance$)
                               :else (. emptyCxt instance$)))
        active (filter #(in? (:active state) %) (all-mites state))
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (.cxt %)) (to-linked-map (.args %)) nil nil nil) active)]
    (new Chart kotlin-mites)))

(defn all-tree-mites [tree] (.mites tree))
(defn tree-core [tree]
  (if (.right-child tree)
    (tree-core (if (is-left-headed? (.root tree)) (.left-child tree) (.right-child tree)))
    tree))
(defn visible-simple-trees [tree]
  (if-let [right (.right-child tree)]
    (concat (visible-simple-trees right)
            (if (is-left-headed? (.root tree))
              [(tree-core tree) tree]
              [tree]))
    [tree]))
(defn visible-tree-mites [tree] (mapcat #(.mites %) (visible-simple-trees tree)))
(defn visible-mites [state] (visible-tree-mites (first (.trees state))))

(defn present-mite [mite active] (str (if active "*" "") (if (is-happy? mite) "" "!") mite))
(defn present-tree [tree]
  (let [active (.active tree)
        present-level (fn [tree]
                        (str "  "
                             (clojure.string/join " " (map #(present-mite % (in? active %)) (.mites tree)))))
        ]
    (clojure.string/join "\n" (map present-level (visible-simple-trees tree)))
  ))

(defn presentable [state]
  (let [visible (visible-mites state)
        active (set (mapcat #(.active %) (.trees state)))
        additional (filter #(and (in? active %) (not (is-happy? %)) (not (in? visible %))) (all-mites state))
        additional-str (if (empty? additional) "" (str "\n    unhappy: " (clojure.string/join " " (map #(present-mite % (in? active %)) additional))))]
    (str (clojure.string/join "\n" (map present-tree (.trees state))) additional-str)))

(defn find-contradictors [mite coll] (filter #(and (not= mite %) (mites-contradict mite %)) coll))
(defn contradictors [tree mite] (get (:contradictor-map tree) mite))
(defn happy-contradictors [tree mite] (filter #(is-happy? %) (contradictors tree mite)))

(defrecord ActiveChange [chosen remaining uncovered])

(defn is-uncovered? [mite tree chosen-map]
  (and
    (every? #(= false (get chosen-map %)) (happy-contradictors tree mite))
    (or
      (= false (get chosen-map mite))
      (not (is-happy? mite)))
    ))

(defn update-uncovered [expelled-coll tree ac]
  (let [suspicious (clojure.set/union (set (mapcat #(contradictors tree %) expelled-coll)) expelled-coll)
        fresh-uncovered (filter #(is-uncovered? % tree (:chosen ac)) suspicious)]
    (assoc ac :uncovered (clojure.set/union (:uncovered ac) fresh-uncovered))))

(defn is-complete-change? [ac] (empty? (:remaining ac)))
(defn fork-change [tree ac]
  (let [mite (first (:remaining ac))
        rest-remaining (rest (:remaining ac))
        taken (let [to-expel (filter #(mites-contradict mite %) rest-remaining)
                    expelled-map (reduce #(assoc %1 %2 false) (:chosen ac) to-expel)
                    ]
                (update-uncovered to-expel tree (assoc ac :chosen (assoc expelled-map mite true) :remaining (filter #(not (in? to-expel %)) rest-remaining))))
        omitted (update-uncovered [mite] tree (assoc ac :chosen (assoc (:chosen ac) mite false) :remaining rest-remaining))]
    [taken omitted]))

(defn apply-change [ac tree]
  (let [all (.mites tree)
        uncovered (:uncovered ac)
        new-active (filter
                     #(or (in? uncovered %) (= true (get (:chosen ac) %))) all)]
    new-active))

(defn build-contradictor-cache [tree]
  (let [all (all-tree-mites tree)]
    (zipmap all (map #(find-contradictors % all) all))))

(defn suggest-active [tree]
  (let [visible (visible-tree-mites tree)
        all (:mites tree)
        invisible (set (filter #(not (in? visible %)) all))
        all-unhappy (filter #(not (is-happy? %)) all)
        all-happy (filter #(is-happy? %) all)
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
          (apply-change next-ac tree)
          (let [forked (fork-change tree next-ac)
                queue (reduce #(assoc %1 %2 (change-weight %2)) queue forked)]
            (recur queue))))
      ))
  )

(defn init-tree [root state]
  (loop [all-mites [] to-enrich [root]]
    (if (empty? to-enrich)
      (assoc (empty-tree root) :mites (vec all-mites))
      (recur (concat all-mites to-enrich)
             (flatten (map (.enrich state) to-enrich))))))

(defn new-leaf-tree [^mites.Mite root ^ParsingState state]
  (let [tree (init-tree root state)
        tree (assoc tree :contradictor-map (build-contradictor-cache tree))
        active (suggest-active tree)]
    (assoc tree :active active)))

(defn do-merge-trees [state ^Tree left-tree ^Tree right-tree]
  (let [all-unified (flatten (for [right (visible-tree-mites right-tree)
                                   left (visible-tree-mites left-tree)]
                               (if-let [unified (unify left right)] [unified] ())
                               ))]
    (for [merged all-unified]
      (let [left-headed (is-left-headed? merged)
            merged-tree (init-tree merged state)
            merged-tree (assoc merged-tree :left-child left-tree :right-child right-tree)
            active (suggest-active merged-tree)
            merged-tree (assoc merged-tree :active active)
            ]
        merged-tree))))

(defn merge-trees [state]
  (let [[right & prev-trees] (:trees state)]
    (if (empty? prev-trees)
      state
      (let [all-trees (do-merge-trees state (first prev-trees) right)
            all-states (map #(assoc state :trees (cons % (rest prev-trees))) all-trees)]
        (if (empty? all-states) state (merge-trees (first all-states)))))))

;(Thread/sleep 10000)

(defn add-word [state mite]
  (let [state (append-log state "\n---------------------------------")
        leaf-tree (new-leaf-tree mite state)
        state (assoc state :trees (cons leaf-tree (.trees state)))
        state (merge-trees state)
        ]
    (append-log state (presentable state)))
  )
