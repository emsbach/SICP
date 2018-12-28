#lang sicp

(define (inc x) (+ x 1))
(define (id x) x)
(define (round-to-next-even x) (+ x (remainder x 2)))
(define (round-to-next-odd x) (+ x (remainder (+ x 1) 2)))


(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (approx-pi accuracy)
  (* 4.0
     (/ (product round-to-next-even 2 inc (+ accuracy 1))
        (product round-to-next-odd 2 inc (+ accuracy 1)))))

(approx-pi 6)
(approx-pi 10000)