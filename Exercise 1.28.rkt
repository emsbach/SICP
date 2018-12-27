#lang sicp

(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (id x) x)

(define (sum-integers-iter a b)
  (define (iter c sum)
    (if (<= c b)
        (iter (+ c 1) (+ sum c))
        sum))
  (iter a 0))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cubes-2 a b)
  (sum cube a inc b))

(define (sum-integers-2 a b)
  (sum id a inc b))

(define (pi-sum-2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(sum-cubes-2 1 10)
(sum-integers-2 1 10)
(* 8 (pi-sum-2 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.001)
