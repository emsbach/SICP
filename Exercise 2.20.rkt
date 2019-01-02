#lang sicp

(define (select predicate l)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (if (predicate (car items))
                  (cons (car items) result)
                  result))))
  (reverse (iter l '())))

(define (same-parity x . y)
  (let ((same-parity? (if (even? x)
                         even?
                         odd?)))
    (select same-parity? (append (list x) y))))

; Testing
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
(display (same-parity 7))

