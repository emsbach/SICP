#lang sicp

(define (inc x) (+ x 1))

(define (cont-frac n d k)
 (define (i-term-cont-frac i)
   (if (> i k)
       0
       (/ (n i)
          (+ (d i) (i-term-cont-frac (inc i))))))
  (i-term-cont-frac 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i)
                         (+ result (d i))))))
  (iter k 0))


(define (approx-e)
  (define (n i) 1.0)
  (define (d i)
    (let ((x (- i 2)))
      (if (< 0 (remainder x 3))
        1.0
        (+ 2 (- i (/ x 3))))))
  (+ 2
     (let ((k 100))
       (cont-frac-iter n d k))))

(approx-e)