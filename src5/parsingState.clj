(ns parsingState
  (:import (cons4 Chart)
           (cons4.constructions sem semSectionEnd emptyCxt)
           (java.util LinkedHashMap))
  (:require [mites :refer :all])
  (:require [schema.core :as s])
  (:require [schema.macros :as sm])
  (:use clojure.data.priority-map)
  )

(defn in? [seq elm] (some #(= elm %) seq))

(defrecord Tree [node left right active contradictor-map]
  Object (toString [_] (str node " from " left " and " right)))

(sm/defrecord ParsingState [trees :- [Tree]
                            log :- String
                            enrich])
(defn empty-parsing-state [enrich] (->ParsingState () "" enrich))

(defrecord Node [root mites]
  Object (toString [_] (str root)))

(defn visible-node-mites [^Tree tree ^Node node]
  (let [left-mites (apply concat (map #(.mites (first %)) (.left node)))
        right-mites (apply concat (map #(.mites (first %)) (.right node)))]
    (concat (.mites node) left-mites right-mites))
  )
(defn all-tree-mites [^Tree tree]
  (let [own (.mites (.node tree))]
    (if (.left tree) (concat (all-tree-mites (.left tree)) (all-tree-mites (.right tree)) own) own)))

(defn append-log [state newLog]
  (assoc state :log
    (str (.log state) newLog "\n")))
(defn print-log [state] (println (str "Log:" (:log state))))
(defn all-mites [state] (flatten (reverse (map #(all-tree-mites %) (.trees state)))))
(defn get-chart [state]
  (let [kotlin-cxt (fn [cxt] (cond
                               (= cxt :sem) (. sem instance$)
                               (= cxt :semSectionEnd) (. semSectionEnd instance$)
                               :else (. emptyCxt instance$)))
        active-set (set (mapcat #(.active %) (.trees state)))
        active (filter active-set (all-mites state))
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (.cxt %)) (to-linked-map (.args %)) nil nil nil) active)]
    (new Chart kotlin-mites)))

(sm/defn visible-nodes [tree :- Tree, direction]
  (let [own-node (.node tree)]
    (if (.left tree)
      (let [side-mites (if direction (visible-nodes (direction tree) direction) [])
            head-key (if (is-left-headed? (.root own-node)) :left :right)
            head-mites (if (not= head-key direction) (visible-nodes (head-key tree) nil) [])
            ]
        (concat side-mites head-mites [own-node]))
      [own-node])
    )
  )

(sm/defn visible-tree-mites [tree :- Tree, direction] (mapcat #(.mites %) (visible-nodes tree direction)))
(defn visible-mites [state] (visible-tree-mites (first (.trees state)) :right))

(defn present-mite [mite active] (str (if active "*" "") (if (is-happy? mite) "" "!") mite))
(defn present-tree [tree]
  (let [active (.active tree)
        present-level (fn [node]
                        (str "  "
                             (clojure.string/join " " (map #(present-mite % (in? active %)) (.mites node)))))
        ]
    (clojure.string/join "\n" (map present-level (visible-nodes tree :right)))
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
(defn include-mite [ac tree mite]
  (let [rest-remaining (remove (set mite) (.remaining ac))
        to-expel (filter #(mites-contradict mite %) rest-remaining)
        expelled-map (reduce #(assoc %1 %2 false) (:chosen ac) to-expel)
        ]
    (update-uncovered to-expel tree (assoc ac :chosen (assoc expelled-map mite true) :remaining (filter #(not (in? to-expel %)) rest-remaining)))))
(defn omit-mite [ac tree mite]
  (update-uncovered [mite] tree (assoc ac :chosen (assoc (:chosen ac) mite false) :remaining (remove (set mite) (.remaining ac)))))

(defn fork-change [tree ac]
  (let [mite (first (:remaining ac))]
    [(include-mite ac tree mite) (omit-mite ac tree mite)]))

(defn apply-change [ac tree]
  (let [all (all-tree-mites tree)
        uncovered (:uncovered ac)
        new-active (filter
                     #(or (in? uncovered %) (= true (get (:chosen ac) %))) all)]
    new-active))

(defn build-contradictor-cache [tree]
  (let [all (all-tree-mites tree)]
    (zipmap all (map #(find-contradictors % all) all))))

(defn all-roots [tree]
  (let [root (.root (.node tree))]
    (if (.left tree) (concat [root] (all-roots (.left tree)) (all-roots (.right tree))) [root])))

(defn suggest-active [tree]
  (let [visible (visible-tree-mites tree :right)
        all (all-tree-mites tree)
        invisible (set (filter #(not (in? visible %)) all))
        all-unhappy (filter #(not (is-happy? %)) all)
        all-happy (filter #(is-happy? %) all)
        spine (all-roots tree)
        spine-ancestors (set (mapcat mite-ancestors spine))
        spine-independent (remove spine-ancestors spine)
        change-weight (fn [ac]
                        (let [uncovered (:uncovered ac)
                              invisible-uncovered (count (filter #(in? invisible %) uncovered))]
                          [invisible-uncovered (- (count uncovered) invisible-uncovered)]))
        initial-change (->ActiveChange {} all-happy #{})
        initial-change (loop [ac initial-change
                              [mite & tail] spine-independent]
                         (if (nil? mite)
                           ac
                           (if (not-empty (find-contradictors mite (filter (.chosen ac) (keys (.chosen ac)))))
                             nil
                             (if-let [ac (include-mite ac tree mite)] (recur ac tail))))
                         )
        ]
    (if (nil? initial-change) nil
      (loop [queue (priority-map initial-change (change-weight initial-change))]
      (let [[next-ac & weight] (peek queue)
            queue (pop queue)]
        (if (is-complete-change? next-ac)
          (apply-change next-ac tree)
          (let [mite (first (:remaining next-ac))
                forked [(include-mite next-ac tree mite) (omit-mite next-ac tree mite)]
                queue (reduce #(assoc %1 %2 (change-weight %2)) queue forked)]
            (recur queue))))
      )))
  )

(defn init-node [root enrich head-node child-node]
  (loop [all-mites [] to-enrich [root]]
    (if (empty? to-enrich)
      (->Node root (vec all-mites))
      (recur (concat all-mites to-enrich)
             (flatten (map #(enrich % head-node child-node) to-enrich))))))

(defn new-tree [^Node node ^Tree left ^Tree right]
  (let [tree (->Tree node left right {} #{})
        tree (assoc tree :contradictor-map (build-contradictor-cache tree))
        active (suggest-active tree)]
    (if active (assoc tree :active active))))

(defn mergeable-combinations [left-tree right-tree]
  (let [left-mites (visible-tree-mites left-tree nil)
        right-mites (visible-tree-mites right-tree nil)]
    (apply concat
         (for [left-mite left-mites, right-mite right-mites]
           (if-let [unified (unify left-mite right-mite)] [unified] ())
           ))))

(sm/defn do-merge-trees :- [[Tree]]
  [state, left-tree :- Tree, right-tree :- Tree, dig-left dig-right take-own-mites]
  (let [own-merged-mites (if take-own-mites (mergeable-combinations left-tree right-tree) [])
        maybe-new-tree (fn [node left right] (if-let [tree (new-tree node left right)] [[tree]] []))
        own-merged-trees (apply concat (for [merged-mite own-merged-mites]
                           (maybe-new-tree (init-node merged-mite (.enrich state) left-tree right-tree) left-tree right-tree)))
        wrap-fun (sm/fn wrap-fun :- [[Tree]]
                   [digging-left, tree-or-two :- [Tree]]
                   (let [main-nested (if digging-left (first tree-or-two) (last tree-or-two))
                         another-nested (if digging-left (second tree-or-two) (second (reverse tree-or-two)))
                         ]
                     (if (= digging-left (is-left-headed? (.root (.node main-nested))))
                       (if-let [tree (new-tree (if digging-left (.node left-tree) (.node right-tree))
                                               (if digging-left (.left left-tree) (.right right-tree))
                                               main-nested)]
                         [(if another-nested
                            (if digging-left [tree another-nested] [another-nested tree])
                            [tree])]
                         [])
                       (if another-nested
                         []
                         [(if digging-left [(.left left-tree) main-nested] [main-nested (.right right-tree)])]
                         )
                       )))
        left-nested (if (and dig-left (.right left-tree))
                      (do-merge-trees state (.right left-tree) right-tree true false (is-left-headed? (.root (.node left-tree))))
                      [])
        left-wrapped (mapcat #(wrap-fun true %) left-nested)
        right-nested (if (and dig-right (.left right-tree))
                       (do-merge-trees state left-tree (.left right-tree) false true (not (is-left-headed? (.root (.node right-tree)))))
                       [])
        right-wrapped (mapcat #(wrap-fun false %) right-nested)
        ]
    (concat left-wrapped right-wrapped own-merged-trees)
    )
)

(defn all-merged-states [state max-count]
  (let [[right & [left & prev-trees]] (.trees state)]
    (if (nil? left)
      [state]
      (let [all-trees (do-merge-trees state left right true true true)
            filtered (filter #(<= (count %) max-count) all-trees)
            merged-states (apply concat (for [tree-or-two filtered]
                                          (all-merged-states (assoc state :trees (concat (reverse tree-or-two) prev-trees))
                                                             (if (= (count tree-or-two) 2) 1 2))
                                          ))
            ]
        (cons state merged-states))
      )))

(defn merge-trees [state]
  (let [variants (all-merged-states state 2)
        sorted (sort-by #(count (.trees %)) variants)]
    (first sorted)))

(defn add-tree [state tree]
  (let [state (append-log state "\n---------------------------------")
        state (assoc state :trees (cons tree (.trees state)))
        state (merge-trees state)]
    (append-log state (presentable state))))

(defn add-word [state mite] (add-tree state (new-tree (init-node mite (.enrich state) nil nil) nil nil)))

(s/set-fn-validation! true)
