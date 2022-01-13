#lang sicp
#|(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))|#

;d change representation

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (is-mobile? structure)
  (pair? structure))

;a make selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  ;change cdr to cadr if commented out constructors are used
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  ;change cdr to cadr if commented out constructors are used
  (cdr branch))

;b get total weigth of mobile
(define (total-weight mobile)
  (if (not (is-mobile? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
  )
)

;c measure torque and check for balance of mobile

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (is-mobile-balanced? mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))

;test
(define branch1 (make-branch 10 20))
(define mobile1 (make-mobile branch1 branch1))
(define mobile2 (make-mobile (make-branch 10 mobile1) (make-branch 10 mobile1)))
(define mobile3 (make-mobile (make-branch 10 mobile2) (make-branch 10 mobile2)))
(define mobile4 (make-mobile (make-branch 10 mobile3) (make-branch 10 mobile2)))

(total-weight mobile1)
(total-weight mobile2)
(total-weight mobile3)
(total-weight mobile4)

(is-mobile-balanced? mobile3)
(is-mobile-balanced? mobile4)

