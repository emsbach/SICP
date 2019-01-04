#lang sicp

; Binary-Mobile 
(define (make-mobile left right) (list left right))
(define left-branch car)
(define right-branch cadr)

; Branch
(define (make-branch length structure) (list length structure))
(define branch-length car)
(define branch-structure cadr)

; Binary-Mobile 2
(define (make-mobile-2 left right) (cons left right))
(define right-branch-2 cdr)

; Branch 2
(define (make-branch-2 length structure) (list length structure))
(define branch-structure-2 cdr)


(define (branch-weight b)
    (let ((structure (branch-structure b)))
      (if (not (pair? structure))
          structure
          (total-weight structure))))

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (torque b)
  (* (branch-length b)
     (branch-weight b)))

(define (balanced? m)
  (let ((lbs (branch-structure (left-branch m)))
        (rbs (branch-structure (right-branch m))))
    (and (= (torque (left-branch m))
            (torque (right-branch m)))
         (if (pair? lbs)
             (balanced? lbs)
             #t)
         (if (pair? rbs)
             (balanced? rbs)
             #t))))

; Testing
(define m1 (make-mobile (make-branch 3
                                    (make-mobile (make-branch 5
                                                              1)
                                                 (make-branch 1
                                                              7)))
                        (make-branch 15
                                    3)))

(define m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4)))))

(define m3 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8)))))

(total-weight m3)
(balanced? m3)
(display m3)
