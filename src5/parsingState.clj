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

(defrecord Node [root mites contradictor-map left right]
  Object (toString [_] (str root)))
(defn empty-node [root] (->Node root [] {} [] []))

(defrecord Tree [root nodes active]
  Object (toString [_] (str root " " nodes)))
(defn get-node [tree mite]
  (let [result ((.nodes tree) mite)]
    (assert result
            (str "no " mite " in " tree))
    result)
  )
(defn root-node [tree] (get-node tree (.root tree)))

(defn visible-node-mites [^Tree tree ^Node node]
  (let [left-mites (apply concat (map #(.mites (first %)) (.left node)))
        right-mites (apply concat (map #(.mites (first %)) (.right node)))]
    (concat (.mites node) left-mites right-mites))
  )
(defn all-tree-mites [tree]
  (letfn [(inner [node]
                 (let [left-mites (apply concat (map #(inner (get-node tree (second %))) (.left node)))
                       right-mites (apply concat (map #(inner (get-node tree (second %))) (.right node)))]
                   (concat left-mites (visible-node-mites tree node) right-mites))
                 )]
    (inner (root-node tree))))

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
        active (filter #(in? (:active state) %) (all-mites state))
        to-linked-map (fn [clj-map]
                        (let [str-keys (map #(name %) (keys clj-map))]
                          (new LinkedHashMap (zipmap str-keys (vals clj-map)))))
        kotlin-mites (map #(new cons4.Mite (kotlin-cxt (.cxt %)) (to-linked-map (.args %)) nil nil nil) active)]
    (new Chart kotlin-mites)))

(defn visible-nodes [tree from-right]
  (letfn [(inner [node]
                 (let [res [node]
                       [next-edge & _] (if from-right (.right node) (.left node))
                       res (if next-edge
                             (concat (inner (get-node tree (second next-edge))) res)
                             res)]
                   res))]
    (inner (root-node tree))))

(defn visible-tree-mites [tree] (mapcat #(visible-node-mites tree %) (visible-nodes tree true)))
(defn visible-mites [state] (visible-tree-mites (first (.trees state))))

(defn present-mite [mite active] (str (if active "*" "") (if (is-happy? mite) "" "!") mite))
(defn present-tree [tree]
  (let [active (.active tree)
        present-level (fn [node]
                        (str "  "
                             (clojure.string/join " " (map #(present-mite % (in? active %)) (visible-node-mites tree node)))))
        ]
    (clojure.string/join "\n" (map present-level (visible-nodes tree true)))
  ))

(defn presentable [state]
  (let [visible (visible-mites state)
        active (set (mapcat #(.active %) (.trees state)))
        additional (filter #(and (in? active %) (not (is-happy? %)) (not (in? visible %))) (all-mites state))
        additional-str (if (empty? additional) "" (str "\n    unhappy: " (clojure.string/join " " (map #(present-mite % (in? active %)) additional))))]
    (str (clojure.string/join "\n" (map present-tree (.trees state))) additional-str)))

(defn find-contradictors [mite coll] (filter #(and (not= mite %) (mites-contradict mite %)) coll))
(defn contradictors [node mite] (get (:contradictor-map node) mite))
(defn happy-contradictors [node mite] (filter #(is-happy? %) (contradictors node mite)))

(defrecord ActiveChange [chosen remaining uncovered])

(defn is-uncovered? [mite node chosen-map]
  (and
    (every? #(= false (get chosen-map %)) (happy-contradictors node mite))
    (or
      (= false (get chosen-map mite))
      (not (is-happy? mite)))
    ))

(defn update-uncovered [expelled-coll node ac]
  (let [suspicious (clojure.set/union (set (mapcat #(contradictors node %) expelled-coll)) expelled-coll)
        fresh-uncovered (filter #(is-uncovered? % node (:chosen ac)) suspicious)]
    (assoc ac :uncovered (clojure.set/union (:uncovered ac) fresh-uncovered))))

(defn is-complete-change? [ac] (empty? (:remaining ac)))
(defn fork-change [node ac]
  (let [mite (first (:remaining ac))
        rest-remaining (rest (:remaining ac))
        taken (let [to-expel (filter #(mites-contradict mite %) rest-remaining)
                    expelled-map (reduce #(assoc %1 %2 false) (:chosen ac) to-expel)
                    ]
                (update-uncovered to-expel node (assoc ac :chosen (assoc expelled-map mite true) :remaining (filter #(not (in? to-expel %)) rest-remaining))))
        omitted (update-uncovered [mite] node (assoc ac :chosen (assoc (:chosen ac) mite false) :remaining rest-remaining))]
    [taken omitted]))

(defn apply-change [ac tree]
  (let [all (all-tree-mites tree)
        uncovered (:uncovered ac)
        new-active (filter
                     #(or (in? uncovered %) (= true (get (:chosen ac) %))) all)]
    new-active))

(defn build-contradictor-cache [node]
  (let [all (.mites node)]
    (zipmap all (map #(find-contradictors % all) all))))

(defn suggest-active [tree]
  (let [visible (visible-tree-mites tree)
        all (all-tree-mites tree)
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

(defn init-node [root enrich]
  (loop [all-mites [] to-enrich [root]]
    (if (empty? to-enrich)
      (assoc (empty-node root) :mites (vec all-mites))
      (recur (concat all-mites to-enrich)
             (flatten (map enrich to-enrich))))))

(defn new-leaf-tree [root-mite enrich]
  (let [node (init-node root-mite enrich)
        node (assoc node :contradictor-map (build-contradictor-cache node))
        tree (->Tree root-mite {root-mite node} #{})
        active (suggest-active tree)]
    (assoc tree :active active)))

(defn do-merge-trees [state ^Tree left-tree ^Tree right-tree]
  (let [left-visible (visible-nodes left-tree true)
        right-visible (visible-nodes right-tree false)
        left-top (root-node left-tree)
        right-top (root-node right-tree)
        merge-node-mites (fn [^Node left-node ^Node right-node]
                           (flatten
                             (for [right (visible-node-mites right-tree right-node)
                                          left (visible-node-mites left-tree left-node)]
                                      (if-let [unified (unify left right)] [unified] ())
                                      )))
        create-merged-tree (fn [merged-mite child-node head-node direction]
                             (let [merged-node (init-node merged-mite (.enrich state))
                                   head-mite (.root head-node)
                                   head-node (assoc head-node direction (cons [merged-node (.root child-node)] (direction head-node)))
                                   nodes-map (merge (.nodes left-tree) (.nodes right-tree))
                                   nodes-map (assoc nodes-map merged-mite merged-node head-mite head-node)
                                   merged-tree (assoc (if (= :right direction) left-tree right-tree) :nodes nodes-map)
                                   active (suggest-active merged-tree)
                                   ]
                               (assoc merged-tree :active active))
                             )
        right-headed-merges (for [right-node right-visible
                                 merged (merge-node-mites left-top right-node) :when (not (is-left-headed? merged))]
                             (create-merged-tree merged left-top right-node :left))
        left-headed-merges (for [left-node left-visible
                                  merged (merge-node-mites left-node right-top) :when (is-left-headed? merged)]
                              (create-merged-tree merged right-top left-node :right))
        ]
    (concat left-headed-merges right-headed-merges)))

(defn merge-trees [state]
  (let [[right & [left & prev-trees]] (.trees state)]
    (if (nil? left)
      state
      (let [all-trees (do-merge-trees state left right)
            all-states (map #(assoc state :trees (cons % (rest prev-trees))) all-trees)]
        (if (empty? all-states) state (merge-trees (first all-states)))))))

(defn add-tree [state tree]
  (let [state (append-log state "\n---------------------------------")
        state (assoc state :trees (cons tree (.trees state)))
        state (merge-trees state)]
    (append-log state (presentable state))))

(defn add-word [state mite] (add-tree state (new-leaf-tree mite (.enrich state))))
