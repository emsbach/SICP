#lang sicp

(define (average a b) (/ (+ a b) 2.0))
(define (inc x) (+ x 1))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess line)
    (let ((next (f guess)))
      (display line)
      (display "  ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (inc line)))))
  (try first-guess 1))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (x-to-the-power-of-x guess)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               guess))

(define (x-to-the-power-of-x-a guess)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               guess))

(x-to-the-power-of-x 2.0)
(x-to-the-power-of-x-a 2.0)