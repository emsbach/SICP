#lang sicp

(define (square x) (* x x))
(define (even? x) (= 0 (remainder x 2)))


(define (miller-rabin-test n)
  (define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e) (remainder (square (expmod b (/ e 2) m))
                              m))
        (else (remainder (* b (expmod b (- e 1) m))
                         m))))
  
  (define (iter a)
    (cond ((= a 0) #t)
          ((= 1 (expmod a (- n 1) n)) (iter (- a 1)))
          (else #f)))
  (iter (- n 1)))


(define (miller-rabin-test-2 n)
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

(miller-rabin-test 1105)
(miller-rabin-test-2 561)
