#lang sicp
(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc items)
  (cond ((not (null? items)) (proc (car items))
                             (for-each proc (cdr items)))))

(define (length x)
  (if (null? x)
      0
      (+ 1 (length (cdr x)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))

; Testing
(define x (list (list 1 2) (list 3 4)))
(display x)
(newline)
(display (deep-reverse x))
(newline)

