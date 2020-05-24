(defrecord Graph [vertices edges])
(defrecord Edge [from to weight label])
(defrecord Vertex [label neighbors latitude longitude status distance])

(def ^:const vertex-status-unseen 0)
(def ^:const vertex-status-in-queue 1)
(def ^:const vertex-status-visited 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIORITY QUEUE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord DListNode [prev data priority next])
(defrecord DList [head tail])

(defn dlist-make[]
  (DList. (ref nil) (ref nil)))

(defn dlist-empty? [lst]
  (nil? @(:head lst)))

(defn dlist-first [lst]
  (:data @(:head lst)))

(defn dlist-prepend! [lst data priority]
  (let [new-node (DListNode. (ref nil) data (ref priority)
                             (ref @(:head lst)))]
    (if (dlist-empty? lst)
      (dosync (ref-set (:head lst) new-node)
              (ref-set (:tail lst) new-node))
      (dosync (ref-set (:prev @(:head lst)) new-node)
              (ref-set (:head lst) new-node)))) true)

(defn dlist-append! [lst data priority]
  (let [new-node (DListNode. (ref @(:tail lst)) data
                             (ref priority) (ref nil))]
    (if (dlist-empty? lst)
      (dosync (ref-set (:head lst) new-node)
              (ref-set (:tail lst) new-node))
      (dosync (ref-set (:next @(:tail lst)) new-node)
              (ref-set (:tail lst) new-node)))))

(defn dlist-rest [lst]
  (DList. (ref @(:next @(:head lst)))
          (ref @(:tail lst))))

(defn dlist-rem-first! [lst]
  (when-not (dlist-empty? lst)
    (if (= @(:head lst) @(:tail lst))
      (dosync (ref-set (:head lst) nil)
              (ref-set (:tail lst) nil))
      (dosync (ref-set (:head lst) @(:next @(:head lst)))
              (ref-set (:prev @(:head lst)) nil)))))

(defn dlist-iter [lst]
  (if (not (dlist-empty? lst))
    (loop [node @(:head lst)]
      (println (:data node) ":" (:priority node))
      (if (not (= node @(:tail lst)))
        (recur @(:next node))))))

(defn dlist-insert-priority! [lst data priority]
  (if-not (dlist-empty? lst)
    (let [inserted? (ref false)]
      (loop [current-node @(:head lst)]
        (if (<= priority @(:priority current-node)) 
          (let [new-node (DListNode. (ref nil)
                                     data
                                     (ref priority)
                                     (ref current-node))
                previous-node @(ref @(:prev current-node))]
            (if-not (nil? previous-node)
              (dosync (ref-set (:prev current-node)
                               new-node)
                      (ref-set (:prev new-node)
                               previous-node)
                      (ref-set (:next previous-node)
                               new-node)
                      (ref-set inserted? true))
              (dosync (ref-set (:prev current-node)
                               new-node)
                      (ref-set (:head lst)
                               new-node)
                      (ref-set inserted? true))))
          (if-not (nil? @(:next current-node))
            (recur @(:next current-node)))))
      (if-not @inserted? 
        (dlist-append! lst data priority))) 
    (dlist-prepend! lst data priority))
  true)

(defn dlist-find-and-update! [lst data priority]
  (when-not (dlist-empty? lst)
    (loop [node @(:head lst)]
      (if (= data (:data node))
        (if (< priority @(:priority node))
          (dosync
            (ref-set (:priority node)
                     priority)
            node) 
          false)
        (if-not (nil? @(:next node))
          (recur @(:next node)))))))

(defn previous-exists? [node]
  (if (nil? @(:prev node))
    false true))

(defn next-exists? [node]
  (if (nil? @(:next node))
    false true))

(defn node-swap! [node lst]
  (if-not (= false node)
    (let [prev-node @(:prev node)
          swapped? (ref false)]
      (when-not (nil? prev-node)
        (if (< @(:priority node) 
               @(:priority prev-node))
          (let [temp-next-from-node @(:next node)
                temp-prev-from-prev-node @(:prev prev-node)]
            (dosync (ref-set (:next prev-node)
                             temp-next-from-node)
                    (ref-set (:prev prev-node)
                             node)
                    (ref-set (:next node)
                             prev-node)
                    (ref-set (:prev node)
                             temp-prev-from-prev-node)
                    (ref-set swapped? true))
            (if (previous-exists? node)
              (dosync (ref-set (:next @(:prev node))
                               node)))
            (if-not (previous-exists? node)
              (dosync (ref-set (:head lst)
                               node)))
            (if (next-exists? prev-node)
              (dosync (ref-set (:prev @(:next prev-node))
                               prev-node)))
            (if-not (next-exists? prev-node)
              (dosync (ref-set (:tail lst)
                               prev-node)))))) @swapped?) false))

(defn dlist-bubble-swap! [lst label priority]
  (loop [node (dlist-find-and-update! lst label priority)]
    (if-not (= (node-swap! node lst) false)
      (recur node))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; INITIALIZING THE GRAPH ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-graph [] (Graph. (ref {}) (ref {})))

(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn graph-add-vertex! [graph label lat lon]
  (if-not (graph-has-vertex? graph label)
    (dosync
      (ref-set (:vertices graph)
               (assoc @(:vertices graph)
                      label
                      (Vertex. label (ref '()) lat lon
                               (ref vertex-status-unseen)
                               (ref 0)))) true)
    (do
      (println "Vertex already in the graph") false)))



(defn graph-make-edge-key [from to] (sort (list from to)))

(defn graph-has-edge? [graph from to]
  (contains? @(:edges graph) (graph-make-edge-key from to)))

(defn graph-add-edge! [graph from to label weight]
  (if (and (graph-has-vertex? graph from) (graph-has-vertex? graph to))
    (if-not (graph-has-edge? graph from to)
      (dosync
        (ref-set (:edges graph)
                 (assoc @(:edges graph)
                        (graph-make-edge-key from to)
                        (Edge. from to weight label)))
        (let [neighbors (:neighbors (get @(:vertices graph) from))]
          (ref-set neighbors (cons to @neighbors)))
        (let [neighbors (:neighbors (get @(:vertices graph) to))]
          (ref-set neighbors (cons from @neighbors))) true)
      (do
        (println "Edge already in the graph") false))
    (do (println "Invalid vertices") false)))

(defn vertex-reset-status! [vertex]
  (dosync
    (ref-set (:status vertex) vertex-status-unseen)))

(defn vertex-reset-distance! [vertex]
  (dosync
    (ref-set (:distance vertex) 0)))

(defn vertex-reset-all! [vertex]
  (vertex-reset-status! vertex)
  (vertex-reset-distance! vertex))

(defn graph-reset!
  ([graph]
   (graph-reset! graph vertex-reset-all!))
  ([graph reset-function]
   (doseq [vertex (vals @(:vertices graph))]
     (reset-function vertex))))

(defn graph-vertex-status? [graph label status]
  (= @(:status (get @(:vertices graph) label))
     status))

(defn graph-vertex-visited? [graph label]
  (graph-vertex-status? graph label vertex-status-visited))

(defn graph-vertex-unseen? [graph label]
  (graph-vertex-status? graph label vertex-status-unseen))

(defn graph-vertex-in-queue? [graph label]
  (graph-vertex-status? graph label vertex-status-in-queue))

(defn graph-vertex-unseen-or-in-queue? [graph label]
  (or (graph-vertex-status? graph label vertex-status-unseen)
      (graph-vertex-status? graph label vertex-status-in-queue)))

(defn graph-get-vertex [graph label]
  (get @(:vertices graph) label))

(defn graph-get-edge [graph from to]
  (get @(:edges graph) (graph-make-edge-key from to)))

(defn graph-get-edge-weight
  ([e]
   (:weight e))
  ([graph from to]
   (graph-get-edge-weight (graph-get-edge graph from to))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; DIJKSTRA'S ALGORITHM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn graph-bfs!
  ([graph]
   (graph-bfs! graph (first (keys @(:vertices graph)))))
  ([graph start]
   (graph-bfs! graph start (fn [x] true)))
  ([graph start func]
   (graph-bfs! graph start func (fn [graph queue] (first queue))))
  ([graph start func queue]
   (loop []
     (when-not (dlist-empty? queue)
       (let [current-label (dlist-first queue)
             current-vertex (get @(:vertices graph) current-label)
             current-neighbors @(:neighbors current-vertex)
             unseen-neighbors (filter #(graph-vertex-unseen? graph %1)
                                      current-neighbors)]
         (let [continue? (func current-vertex queue)]
           (dosync (ref-set (:status current-vertex) vertex-status-visited))
           (if continue?
             (recur) (println "Marking stage done"))))))))

(defn vertex-get-best-neighbor [graph vertex]
  (let [best-distance (ref ##Inf)
        best-label (ref "")]
    (doseq [neighbor-label @(:neighbors vertex)]
      (let [neighbor (graph-get-vertex graph neighbor-label)]
        (if (and (= @(:status neighbor) vertex-status-visited)
                 (< @(:distance neighbor) @best-distance))
          (dosync
            (ref-set best-distance @(:distance neighbor))
            (ref-set best-label (:label neighbor))))))
    @best-label))

(defn vertex-get-best-neighbor-with-weights [graph vertex]
  (let [best-distance (ref ##Inf)
        best-label (ref "")]
    (doseq [neighbor-label @(:neighbors vertex)]
      (let [neighbor (graph-get-vertex graph neighbor-label)]
        (when (and (= @(:status neighbor) vertex-status-visited)
                   (< @(:distance neighbor) @best-distance)
                   (= (- @(:distance vertex) @(:distance neighbor))
                      (graph-get-edge-weight graph
                                             (:label vertex)
                                             (:label neighbor))))
          (dosync
            (ref-set best-distance @(:distance neighbor))
            (ref-set best-label (:label neighbor))))))
    @best-label))

(defn graph-trace-back [graph start finish best-neighbor-func]
  (let [start-vertex (graph-get-vertex graph start)
        ret-lst (ref '())]
    (if (= @(:status start-vertex) vertex-status-visited)
      (loop [current-label start]
        (if (not (= current-label finish))
          (let [current-vertex (graph-get-vertex graph current-label)]
            (println ">>" current-label "::" @(:distance current-vertex))
            (dosync (ref-set ret-lst
                             (conj @ret-lst current-label))
                    (ref-set (:status current-vertex) vertex-status-unseen))
            (recur (best-neighbor-func graph current-vertex)))
          (do
            (println "**" current-label)
            (println "Arrived at finish!")
            )))
      (do
        (newline)
        (println "Path does not exist!"))) (reverse @ret-lst)))

(defn graph-dijkstra-helper! [graph start finish]
  (graph-reset! graph)
  (dosync
    (ref-set (:distance (graph-get-vertex graph finish)) 0))
  (let [queue (dlist-make)
        cnt (ref 0)]
    (dlist-prepend! queue finish @(:distance (graph-get-vertex graph finish)))
    (graph-bfs! graph
                finish
                (fn [vertex queue]
                  (dlist-rem-first! queue)
                  (if (= start (:label vertex))
                    false
                    (if-not (= @(:status vertex) vertex-status-visited)
                      (do
                        (dosync (ref-set cnt (inc @cnt)))
                        (doseq [neighbor-label
                                (filter (fn [label]
                                          (graph-vertex-unseen-or-in-queue? graph label))
                                        @(:neighbors vertex))]
                          (let [neighbor (graph-get-vertex graph neighbor-label)]
                            (dosync
                              (if (or (> @(:distance neighbor)
                                         (inc @(:distance vertex)))
                                      (= @(:distance neighbor) 0))
                                (ref-set (:distance neighbor)
                                         (inc @(:distance vertex))))
                              (if (graph-vertex-in-queue? graph neighbor-label)
                                (dlist-bubble-swap! queue neighbor-label @(:distance neighbor))
                                (dlist-insert-priority! queue neighbor-label @(:distance neighbor)))
                              (ref-set (:status neighbor) vertex-status-in-queue))
                            ))
                        true)))) queue)
    (println "Vertices visited:" @cnt)
    (newline)
    (graph-trace-back graph start finish vertex-get-best-neighbor)))

(defn graph-a*-helper! [graph start finish]
  (graph-reset! graph)
  (let [queue (dlist-make)
        cnt (ref 0)]
    (dlist-prepend! queue finish @(:distance (graph-get-vertex graph finish)))
    (graph-bfs! graph
                start
                (fn [vertex queue]
                  (dlist-rem-first! queue)
                  (if (= finish (:label vertex))
                    false
                    (if-not (= @(:status vertex) vertex-status-visited)
                      (do
                        (dosync (ref-set cnt (inc @cnt)))
                        (if (= finish (:label vertex))
                          false
                          (do
                            (doseq [neighbor-label
                                    (filter
                                      (fn [label]
                                        (graph-vertex-unseen-or-in-queue? graph label))
                                      @(:neighbors vertex))]
                              (let [neighbor (graph-get-vertex graph neighbor-label)
                                    weight (graph-get-edge-weight graph
                                                                  (:label vertex)
                                                                  neighbor-label)
                                    distance (+ (- @(:distance vertex) 
                                                   (graph-great-circle-distance graph (:label vertex) finish))
                                                weight (graph-great-circle-distance graph neighbor-label finish))]

                                (dosync (ref-set (:status neighbor) vertex-status-in-queue))
                                (when (or (= @(:distance neighbor) 0)
                                          (< distance @(:distance neighbor)))
                                  (dosync
                                    (ref-set (:distance neighbor)
                                             distance)))
                                (dlist-insert-priority! queue neighbor-label @(:distance neighbor)) 
                                ))
                            true))) true)))
                queue)
    (println "Vertices visited:" @cnt)
    (newline)
    (graph-trace-back graph finish start
                      vertex-get-best-neighbor-with-weights)
    ))



(defn graph-dijkstra-with-weights-helper! [graph start finish]
  (graph-reset! graph)
  (dosync
    (ref-set (:distance (graph-get-vertex graph finish)) 0))
  (let [queue (dlist-make)
        cnt (ref 0)]
    (dlist-prepend! queue finish @(:distance (graph-get-vertex graph finish)))
    (graph-bfs! graph
                finish
                (fn [vertex queue]
                  (dlist-rem-first! queue)
                  (if (= start (:label vertex))
                    false
                    (if-not (= @(:status vertex) vertex-status-visited)
                      (do
                        (dosync (ref-set cnt (inc @cnt)))
                        (if (= start (:label vertex))
                          false
                          (do
                            (doseq [neighbor-label
                                    (filter
                                      (fn [label]
                                        (graph-vertex-unseen-or-in-queue? graph label))
                                      @(:neighbors vertex))]
                              (let [neighbor (graph-get-vertex graph neighbor-label)
                                    weight (graph-get-edge-weight graph
                                                                  (:label vertex)
                                                                  neighbor-label)
                                    distance (+ @(:distance vertex)
                                                weight)]
                                (when (or (= @(:distance neighbor) 0)
                                          (< distance @(:distance neighbor)))
                                  (dosync
                                    (ref-set (:distance neighbor)
                                             distance)))
                                (if (graph-vertex-in-queue? graph neighbor-label)
                                  (dlist-bubble-swap! queue neighbor-label @(:distance neighbor))
                                  (dlist-insert-priority! queue neighbor-label @(:distance neighbor)))
                                (dosync (ref-set (:status neighbor) vertex-status-in-queue))
                                ))
                            true))) true)))
                queue)
    (println "Vertices visited:" @cnt)
    (newline)
    (graph-trace-back graph start finish
                      vertex-get-best-neighbor-with-weights)
    ))

(defn format-lst [g lst finish]
  (newline)(newline)
  (println "------------------------------------------")
  (println "---------------INSTRUCTIONS---------------")
  (println "------------------------------------------")
  (newline)(newline)
  (let [string (ref '())]
    (dosync
      (ref-set string (str "Start at " (first lst) " and "))
      (loop [x (rest lst)]
        (if (> (count x) 0)
          (do
            (let [f (first x)
                  fr (first (rest x))
                  edge (:label (graph-get-edge g f fr))]
              (if (= (subs (first x) 0 1) "X")
                (do
                  (ref-set string (str @string "take " edge " on the "
                                       (subs f 3 12) " highway crossing. Next, you "))
                  (ref-set string (str @string "choose " edge " in the "
                                       (subs f 3 12) " highway crossing. After passing it, ")))
                (if (even? (count x))
                  (if (even? (rand-int 10))
                    (ref-set string (str @string "via " edge " drive to " f
                                         ". After getting there, "))
                    (ref-set string (str @string "through " edge " continue to " f
                                         ". From " f ", ")))
                  (if (even? (rand-int 10))
                    (ref-set string (str @string "take " edge " and drive to " f
                                         ". Then, "))
                    (ref-set string (str @string "via " edge " continue to " f
                                         ". When you get to " f ", "))))))
            (recur (rest x)))
          (ref-set string (str @string "take the road to " finish
                               " and you will arrive at the finish!")))))
    (println @string)))

(defn graph-dijkstra! [graph start finish]
  (if (and (graph-has-vertex? graph start)
           (graph-has-vertex? graph finish))
    (let [lst (graph-dijkstra-helper! graph start finish)]
      (if-not (empty? lst)
        (format-lst graph lst finish)
        nil))
    "Invalid vertices"))

(defn graph-dijkstra-with-weights! [graph start finish]
  (if (and (graph-has-vertex? graph start)
           (graph-has-vertex? graph finish))
    (let [lst (graph-dijkstra-with-weights-helper! graph start finish)]
      (if-not (empty? lst)
        (format-lst graph lst finish)
        nil))
    "Invalid vertices"))

(defn graph-a*! [graph start finish]
  (if (and (graph-has-vertex? graph start)
           (graph-has-vertex? graph finish))
    (let [lst (graph-a*-helper! graph start finish)]
      (if-not (empty? lst)
        (format-lst graph lst finish)
        nil))
    "Invalid vertices"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A* SEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn graph-bfs-remove-from-queue [queue label]
  (filter (fn [x]
            (not (= x label)))
          queue))

(defn graph-great-circle-distance [graph label1 label2]
  (let [vertex1 (graph-get-vertex graph label1)
        vertex2 (graph-get-vertex graph label2)
        lat1 (:latitude vertex1)
        lon1 (:longitude vertex1)
        lat2 (:latitude vertex2)
        lon2 (:longitude vertex2)
        dl (Math/abs (- lon2 lon1)) ; lambda - longitude
        dp (Math/abs (- lat2 lat1)) ; phi - latitude
        dlr (/ (* Math/PI dl) 180)
        dpr (/ (* Math/PI dp) 180)
        l1 (/ (* Math/PI lon1) 180)
        p1 (/ (* Math/PI lat1) 180)
        l2 (/ (* Math/PI lon2) 180)
        p2 (/ (* Math/PI lat2) 180)
        ds (Math/acos (+ (* (Math/sin p1) (Math/sin p2))
                         (* (Math/cos p1) (Math/cos p2) (Math/cos dlr))))]
    (* 6378 ds)))

(defn graph-a-star-pick-best [graph queue finish]
  (let [best-distance (ref ##Inf)
        best-label (ref "")]
    (doseq [label queue]
      (let [vertex (graph-get-vertex graph label)
            distance-to-finish (graph-great-circle-distance graph
                                                            label
                                                            finish)
            cost-estimation (+ @(:distance vertex) distance-to-finish)]
        (if (< cost-estimation @best-distance)
          (dosync
            (ref-set best-distance cost-estimation)
            (ref-set best-label (:label vertex))))))
    @best-label))

(defn graph-best-first-search!
  ([graph]
   (graph-best-first-search! graph (first (keys @(:vertices graph)))))
  ([graph start]
   (graph-best-first-search! graph start (fn [x] true)))
  ([graph start func]
   (graph-best-first-search! graph start func (fn [graph queue] (first queue))))
  ([graph start func pick-best]
   (loop [queue (list start)]
     (when-not (empty? queue)
       (let [current-label (pick-best graph queue)
             current-vertex (get @(:vertices graph) current-label)
             current-neighbors @(:neighbors current-vertex)
             unseen-neighbors (filter #(graph-vertex-unseen? graph %1)
                                      current-neighbors)]
         (let [continue? (func current-vertex)]
           (dosync (ref-set (:status current-vertex) vertex-status-visited))
           (dosync
             (doseq [neighbor unseen-neighbors]
               (ref-set (:status (get @(:vertices graph) neighbor))
                        vertex-status-in-queue)))
           (if continue?
             (recur (concat (graph-bfs-remove-from-queue queue current-label)
                            unseen-neighbors)))))))))

(defn graph-a-star-helper! [graph finish start]
  (graph-reset! graph)
  (dosync
    (ref-set (:distance (graph-get-vertex graph start)) 0))
  (let [cnt (ref 0)]
    (graph-best-first-search! graph
                              start
                              (fn [vertex]
                                (dosync (ref-set cnt (inc @cnt)))
                                (if (= finish (:label vertex))
                                  false
                                  (do
                                    (doseq [neighbor-label
                                            (filter
                                              (fn [label]
                                                (graph-vertex-unseen-or-in-queue? graph label))
                                              @(:neighbors vertex))]
                                      (let [neighbor (graph-get-vertex graph neighbor-label)
                                            weight (graph-get-edge-weight graph
                                                                          (:label vertex)
                                                                          neighbor-label)
                                            distance (+ @(:distance vertex)
                                                        weight)]
                                        (println distance)
                                        (when (or (= @(:distance neighbor) 0)
                                                  (< distance @(:distance neighbor)))
                                          (dosync
                                            (ref-set (:distance neighbor)
                                                     distance)))))
                                    true)))
                              (fn [graph queue]
                                (graph-a-star-pick-best graph queue finish)))
    (println "Vertices visited:" @cnt)
    (newline))
  (graph-trace-back graph finish start
                    vertex-get-best-neighbor-with-weights))

(defn graph-a-star! [graph start finish]
  (if (and (graph-has-vertex? graph start)
           (graph-has-vertex? graph finish))
    (let [lst (graph-a-star-helper! graph start finish)]
      (if-not (empty? lst)
        (format-lst graph lst finish)
        nil))
    "Invalid vertices"))
