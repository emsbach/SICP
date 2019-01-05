#lang sicp
(define (square x) (* x x))

(define (map-tree proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree))
                    (map-tree proc (cdr tree))))))

(define (scale-tree factor tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree factor (car tree))
                    (scale-tree factor (cdr tree))))))

(define (scale-tree-2 factor tree)
  (map-tree (lambda (x) (* factor x))
            tree))

(define (scale-tree-3 factor tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-3 factor sub-tree)
             (* factor sub-tree)))
       tree))

(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (square subtree)))
       tree))

(define (square-tree-2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-2 (car tree))
                    (square-tree-2 (cdr tree))))))

; Testing
(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(display (square-tree-2 t))