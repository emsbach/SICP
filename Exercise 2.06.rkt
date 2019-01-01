#lang sicp

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
;(lambda (f) (lambda (x) (f (((lambda (y) y) x)))))
;(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;(lambda (f) (lambda (x) (f ((one f) x))))
;(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
;(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

;(add one two)
;(lambda (f) (lambda (x) ((two f) ((one f) x))))
;(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g (g y)))) f) (((lambda (h) (lambda (z) (h z))) f) x))))
;(lambda (f) (lambda (x) ((lambda (y) (f (f y))) ((lambda (z) (f z)) x))))
;(lambda (f) (lambda (x) ((lambda (y) (f (f y))) (f x))))
;(lambda (f) (lambda (x) (f (f (f x)))))
