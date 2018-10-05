#lang racket/base

(require graph racket/match)

(provide
 (all-defined-out)
 (all-from-out graph))

(define (recess-init init-func system-in-out-name-lists recess-graph decon system?)
  ;; user's init expressions
  (init-func)
  ;; build up the dependency graph
  (define display-graph (unweighted-graph/directed '()))
  (define scc-graph (unweighted-graph/directed '()))
  (for-each
   (λ (sys-in-out-name-list)
     (match-define (list in out name) (car sys-in-out-name-list))
     (match-define (list display-in display-out display-name) (cdr sys-in-out-name-list))
     ((add-to-graph recess-graph) name in out)
     ((add-to-scc-graph display-graph) display-name display-in (list))
     ((add-to-scc-graph scc-graph) name in (list)))
   system-in-out-name-lists)
  (displayln (scc display-graph))
  (define new
    (map
     (λ (subg)
       (define g (unweighted-graph/directed '()))
       (for-each
        (λ (sys-in-out-name-list)
          (match-define (list in out name) sys-in-out-name-list)
          ((add-to-graph g) name in out))
        (map car (map decon (filter system? subg))))
       g)
     (scc scc-graph)))
  ;(displayln (display (map graphviz new)))
  (display new)
  (cons recess-graph display-graph))

;; add a system to the dependency graph in the current world
(define (add-to-graph recess-graph)
  (λ (system-name input-events output-events)
    (begin
      (add-vertex! recess-graph system-name)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph ev system-name)))
       input-events)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph system-name ev)))
       output-events))))

(define (add-to-scc-graph recess-graph)
  (λ (system-name input-events output-events)
    (begin
      (add-vertex! recess-graph system-name)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph ev system-name)
           (add-directed-edge! recess-graph system-name ev)))
       input-events)
      (for-each
       (λ (ev)
         (begin
           (add-vertex! recess-graph ev)
           (add-directed-edge! recess-graph system-name ev)))
       output-events))))