#lang sicp
; Interval Library
(define (make-interval l u)
  (if (<= l u)
      (cons l u)
      (cons u l)))
(define lower-bound car)
(define upper-bound cdr)
(define (display-interval i)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]")
  (newline))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
               (make-interval (* -1 (upper-bound y))
                              (* -1 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval-2 x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((<= 0 a)
           (cond ((<= 0 c) (make-interval (* a c) (* b d)))
                 ((and (> 0 c) (< 0 d)) (make-interval (* b c) (* b d)))
                 ((>= 0 d) (make-interval (* b c) (* a d)))))
           ((and (> 0 a) (< 0 b))
            (cond ((<= 0 c) (make-interval (* a d) (* b d)))
                 ((and (> 0 c) (< 0 d)) (make-interval (* b c) (max (* a c) (* b d))))
                 ((>= 0 d) (make-interval (* b c) (* a c)))))
            ((>= 0 b)
             (cond ((<= 0 c) (make-interval (* a d) (* b c)))
                 ((and (> 0 c) (< 0 d)) (make-interval (* a d) (* a c)))
                 ((>= 0 d) (make-interval (* b d) (* a c))))))))

(define (div-interval x y)
  (if (and (<= (* (lower-bound y) (upper-bound y)) 0))
      (error "You cannot divide by an interval that spans zero: " y)
      (mul-interval x
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(define (width i)
  (/ (abs (- (upper-bound i) (lower-bound i)))
     2.0))
(define (width-of-sum a b) (+ (width a) (width b)))
(define (width-of-difference a b)
  (width-of-sum a
                (make-interval (- (upper-bound b))
                               (- (lower-bound b)))))


; Testing
(define (ensure-mult-works aU aL bU bL)
  (define (eql-interval? a b)
    (and (= (lower-bound a) (lower-bound b))
         (= (upper-bound a) (upper-bound b))))
   (let ((a (make-interval aL aU)) 
         (b (make-interval bL bU))) 
   (if (eql-interval? (mul-interval a b) 
                      (mul-interval-2 a b)) 
       true 
       (error "mul-interval-2 returns different value!"  
              a  
              b  
              (mul-interval a b) 
              (mul-interval-2 a b))))) 
 
  
 (ensure-mult-works  +10 +10   +10 +10) 
 (ensure-mult-works  +10 +10   +00 +10) 
 (ensure-mult-works  +10 +10   +00 +00) 
 (ensure-mult-works  +10 +10   +10 -10) 
 (ensure-mult-works  +10 +10   -10 +00) 
 (ensure-mult-works  +10 +10   -10 -10) 
  
 (ensure-mult-works  +00 +10   +10 +10) 
 (ensure-mult-works  +00 +10   +00 +10) 
 (ensure-mult-works  +00 +10   +00 +00) 
 (ensure-mult-works  +00 +10   +10 -10) 
 (ensure-mult-works  +00 +10   -10 +00) 
 (ensure-mult-works  +00 +10   -10 -10) 
  
 (ensure-mult-works  +00 +00   +10 +10) 
 (ensure-mult-works  +00 +00   +00 +10) 
 (ensure-mult-works  +00 +00   +00 +00) 
 (ensure-mult-works  +00 +00   +10 -10) 
 (ensure-mult-works  +00 +00   -10 +00) 
 (ensure-mult-works  +00 +00   -10 -10) 
  
 (ensure-mult-works  +10 -10   +10 +10) 
 (ensure-mult-works  +10 -10   +00 +10) 
 (ensure-mult-works  +10 -10   +00 +00) 
 (ensure-mult-works  +10 -10   +10 -10) 
 (ensure-mult-works  +10 -10   -10 +00) 
 (ensure-mult-works  +10 -10   -10 -10) 
  
 (ensure-mult-works  -10 +00   +10 +10) 
 (ensure-mult-works  -10 +00   +00 +10) 
 (ensure-mult-works  -10 +00   +00 +00) 
 (ensure-mult-works  -10 +00   +10 -10) 
 (ensure-mult-works  -10 +00   -10 +00) 
 (ensure-mult-works  -10 +00   -10 -10) 
  
 (ensure-mult-works  -10 -10   +10 +10) 
 (ensure-mult-works  -10 -10   +00 +10) 
 (ensure-mult-works  -10 -10   +00 +00) 
 (ensure-mult-works  -10 -10   +10 -10) 
 (ensure-mult-works  -10 -10   -10 +00) 
 (ensure-mult-works  -10 -10   -10 -10) 