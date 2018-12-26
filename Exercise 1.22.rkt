#lang sicp

(define (even? n)
  (= 0 (remainder n 2)))

(define (square n) (* n n))

(define (divides? d n)
  (= 0 (remainder n d)))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (is-prime? n)
  (= n (smallest-divisor n)))

(define (start-prime-test n counter start-time)
  (define (report-prime elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))
  
  (cond ((is-prime? n) (report-prime (- (runtime) start-time))
                       (timed-prime-test (+ n 2) (- counter 1)))
        (else (timed-prime-test (+ n 2) counter))))

(define (timed-prime-test n counter)
  (cond ((> counter 0) (start-prime-test n counter (runtime)))))

(define (search-for-primes start counter)
  (cond ((even? start) (search-for-primes (+ start 1) counter))
        ((> counter 0) (timed-prime-test start counter))))


(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
