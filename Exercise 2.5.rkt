#lang sicp

(define (square x) (* x x))
(define (exp b e)
  (cond ((= e 0) 1)
        ((even? e) (square (exp b (/ e 2))))
        (else (* b (exp b (- e 1))))))

(define (cons a b) (* (exp 2 a)
                      (exp 3 b)))
(define (helper prim n counter)
  (if (or (< 0 (remainder n prim)) (= 0 n))
      counter
      (helper prim (/ n prim) (+ counter 1))))
  
(define (car c) (helper 2 c 0))
(define (cdr c) (helper 3 c 0))

(car (cons 1 4))
(cdr (cons 5 0)) 
