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


(define (approx-golden-ratio iter?)
  (define (c1 i) 1.0)
  (/ 1
     (let ((k 1000))
       (if iter?
           (cont-frac-iter c1 c1 k)
           (cont-frac c1 c1 k)))))

(define (sqrt x)
  (cont-frac (lambda (i) (- x 1))
             (lambda (i) (if (= 1 i)
                             1.0
                             2.0))
             10000))

(approx-golden-ratio #f)  ;recursive process
(approx-golden-ratio #t)  ;iterative process
(sqrt 25)