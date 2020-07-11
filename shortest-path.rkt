#lang racket

(require data/heap)

(struct Graph (vertices edges) #:transparent 
  #:mutable)
(struct Edge (from to weight e-label) #:transparent
  #:mutable)
(struct Vertex (label neighbors latitude longitude
                      status distance cost-estimation
                      component) #:transparent
  #:mutable)

(define vertex-status-unseen 0)
(define vertex-status-in-queue 1)
(define vertex-status-visited 2)

(define (make-graph) (Graph (make-hash) (make-hash)))

(define (graph-add-vertex! graph label lat lon)
  (hash-set!
    (Graph-vertices graph)
    label
    (Vertex label '()
            lat lon 
            0 0 0 0)))

(define (graph-make-edge-key lst)
  (sort lst string<?))

(define (graph-add-edge! g from to weight e-label)
  (hash-set!
    (Graph-edges g)
    (graph-make-edge-key (list from to))
    (Edge from to e-label weight ))
  (let* ([vertex-from (graph-get-vertex g from)]
         [vertex-to (graph-get-vertex g to)]
         [neighbors-from (Vertex-neighbors vertex-from)]
         [neighbors-to (Vertex-neighbors vertex-to)])
    (when (not (member from neighbors-to))
      (set-Vertex-neighbors! 
        vertex-to
        (cons from neighbors-to))
      (when (not (member to neighbors-from))
        (set-Vertex-neighbors!
          vertex-from
          (cons to neighbors-from))))))

(define (graph-get-vertex g label)
  (hash-ref (Graph-vertices g) label
            #f))

(define (graph-get-edge g from to)
  (hash-ref (Graph-edges g) (graph-make-edge-key (list from to)) #f))

(define (graph-get-edge-weight g from to)
  (Edge-weight (hash-ref (Graph-edges g) (graph-make-edge-key (list from to)))))

(define (reset-status-helper g lst)
  (when (not (empty? lst))
    (set-Vertex-status! (car lst) 0)
    (reset-status-helper g (cdr lst)))
  "Graph status reset!")

(define (reset-all-helper g lst)
  (when (not (empty? lst))
    (set-Vertex-status! (car lst) 0)
    (set-Vertex-distance! (car lst) 0)
    (reset-all-helper g (cdr lst)))
  "Graph reset!")

(define (reset-all! g)
  (reset-all-helper g (hash-values (Graph-vertices g))))

(define (reset-status! g)
  (reset-status-helper g (hash-values (Graph-vertices g))))

(define (return-unseen-vertex-helper g lst)
  (if (empty? lst) '()
    (let ([current-vertex (graph-get-vertex 
                            g (car lst))])
      (if (= vertex-status-unseen 
             (Vertex-status current-vertex))
        (car lst)
        (return-unseen-vertex-helper g (cdr lst))))))

(define (vertex-unseen? vertex-label)
  (= vertex-status-unseen (Vertex-status
                            (graph-get-vertex g vertex-label))))

(define (vertex-not-visited? vertex-label)
  (not (= vertex-status-visited (Vertex-status 
                                  (graph-get-vertex g vertex-label)))))

(define (return-unseen-vertex g)
  (return-unseen-vertex-helper g 
                               (hash-keys (Graph-vertices g))))

(define (graph-assign-components-helper g ctr queue)
  (when (not (empty? queue)) 
    (let* ([current-label (car queue)]
           [current-vertex 
             (graph-get-vertex g current-label)]
           [current-neighbors (Vertex-neighbors
                                current-vertex)]
           [unvisited-neighbors (foldr
                                  (lambda (v l) 
                                    (if (= (Vertex-status (graph-get-vertex g v))
                                           vertex-status-unseen) 
                                      (cons v l) l)) '() current-neighbors)])
      (set-Vertex-component! current-vertex ctr)
      (set-Vertex-status! current-vertex 1)
      (graph-assign-components-helper g ctr
                                      (append (cdr queue) unvisited-neighbors)))))


(define (graph-assign-components-loop g ctr) 
  (graph-assign-components-helper g ctr
                                  (list (return-unseen-vertex g)))
  (when (not (empty? (return-unseen-vertex g)))
    (graph-assign-components-loop g (add1 ctr))))

(define (graph-assign-components g)
  (reset-all! g)
  (graph-assign-components-loop g 0))

(define (process-neighbors! g distance-so-far lst-of-neighbors queue vertex-label weights)
  (when (not (empty? lst-of-neighbors))
    (let* ([current-neighbor (graph-get-vertex g (car lst-of-neighbors))]
           [current-neighbor-distance (Vertex-distance current-neighbor)])
      (if weights 
        (let ([current-distance (Vertex-distance current-neighbor)]
              [potential-distance (+ distance-so-far (graph-get-edge-weight g
                                                                            (car lst-of-neighbors) vertex-label))])
          (when (or (< potential-distance current-distance)
                    (vertex-unseen? (Vertex-label current-neighbor)))
            (set-Vertex-distance! current-neighbor potential-distance)))
        (when (or (< (add1 distance-so-far) (Vertex-distance current-neighbor))
                  (vertex-unseen? (Vertex-label current-neighbor)))
          (set-Vertex-distance! current-neighbor (add1 distance-so-far))))
      (set-Vertex-status! current-neighbor vertex-status-in-queue)
      (heap-add! queue (list (Vertex-distance current-neighbor) (car lst-of-neighbors)))
      (process-neighbors! g distance-so-far (cdr lst-of-neighbors) queue vertex-label weights))))

(define (graph-bfs! g start queue [weights #f])
  (if (equal? start (cadr (heap-min queue))) 
    (displayln "Marking stage done")
    (let* ([current-label (cadr (heap-min queue))]
           [current-vertex (graph-get-vertex g current-label)]
           [neighbors (Vertex-neighbors current-vertex)]
           [unvisited-neighbors (foldr
                                  (lambda (v l) 
                                    (if (vertex-not-visited? v)
                                      (cons v l) l)) '() neighbors)]
           [distance-so-far (Vertex-distance current-vertex)])
      (heap-remove-min! queue)
      (when (vertex-not-visited? current-label)
        (set-Vertex-status! current-vertex vertex-status-visited)
        (process-neighbors! g distance-so-far unvisited-neighbors queue current-label weights))
      (when (heap-min queue) (graph-bfs! g start queue weights)))))

(define (dijkstra-without-weights! g start finish)
  (reset-all! g)
  (let ([pq (make-heap 
              (lambda (p1 p2) 
                (<= (car p1) (car p2))))])
    (if (= (Vertex-component (graph-get-vertex g start)) (Vertex-component (graph-get-vertex g finish)))
      (begin
        (heap-add! pq (list 0 finish))
        (graph-bfs! g start pq)
        (trace-back g start finish))
      "A path does not exist.")))

(define (dijkstra-with-weights! g start finish)
  (reset-all! g)
  (let ([pq (make-heap 
              (lambda (p1 p2) 
                (<= (car p1) (car p2))))])
    (if (= (Vertex-component (graph-get-vertex g start)) (Vertex-component (graph-get-vertex g finish)))
      (begin
        (heap-add! pq (list 0 finish))
        (graph-bfs! g start pq #t)
        (trace-back g start finish #t))
      "A path does not exist.")))

(define (trace-back-helper g lst-of-neighbors [vertex-label #f])
  (let ([best-distance +inf.0]
        [best-label ""])
    (for ([neighbor lst-of-neighbors])
      (let ([distance (Vertex-distance (graph-get-vertex g neighbor))])
        (if vertex-label
          (when (and 
                  (< distance best-distance)
                  (= (- (Vertex-distance (graph-get-vertex g vertex-label)) distance)
                     (graph-get-edge-weight g neighbor vertex-label)))
            (set! best-distance distance)
            (set! best-label neighbor))
          (when (< distance best-distance)
            (set! best-distance distance)
            (set! best-label neighbor)))))
    best-label))

(define (trace-back g start finish [weights #f])
  (printf "~a: ~a\n" start (Vertex-distance (graph-get-vertex g start)))
  (when (not (equal? start finish))
    (let* ([current-vertex (graph-get-vertex g start)]
           [neighbors (Vertex-neighbors current-vertex)]
           [visited-neighbors (foldr
                                (lambda (v l) 
                                  (if (= (Vertex-status (graph-get-vertex g v))
                                         vertex-status-visited)
                                    (cons v l) l)) '() neighbors)])
      (if weights
        (trace-back g
                    (trace-back-helper g visited-neighbors start)
                    finish)
        (trace-back g
                    (trace-back-helper g visited-neighbors)
                    finish)))))
