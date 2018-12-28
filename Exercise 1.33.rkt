#lang sicp

(define (inc x) (+ x 1))
(define (id x) x)
(define (square x) (* x x))
(define (even? x) (= 0 (remainder x 2)))

(define (miller-rabin-test n)
  (define (expmod b e m)
    (cond ((= e 0) 1)
        ((even? e)
         (if (nontrivial-square-root? (expmod b (/ e 2) m))
             0
             (remainder (square (expmod b (/ e 2) m)) m)))
        (else (remainder (* b (expmod b (- e 1) m))
                         m))))
  (define (nontrivial-square-root? x)
  (and (not (= 1 x))
       (not (= (- n 1) x))
       (= 1 (remainder (square x) n))))
  (define (iter a)
    (cond ((= a 0) #t)
          ((= 0 (expmod a n n)) #f)
          (else (iter (- a 1)))))
  (if (< n 2)
      #f
      (iter (- n 1))))

(define (gcd a b)
    (cond ((< a b) (gcd b a))
          ((= 0 b) a)
          (else (gcd b (remainder a b)))))


(define (filtered-accumulator predicate? combiner null-value term a next b)
  (define (filter x)
    (if (predicate? x)
        (term x)
        null-value))
  (if (> a b)
      null-value
      (combiner (filter a)
                (filtered-accumulator predicate? combiner null-value term (next a) next b))))


(define (sum-of-squares-of-primes a b)
  (filtered-accumulator miller-rabin-test + 0 square a inc b))

(define (relative-prime-product n)
  (define (relative-prime? x)
    (= 1 (gcd n x)))
  (filtered-accumulator relative-prime? * 1 id 0 inc (- n 1)))


(sum-of-squares-of-primes 1 10)
(relative-prime-product 10)