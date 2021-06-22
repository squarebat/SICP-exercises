#lang scheme
;table for data direction
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

;exponent evaluation procedure
(define (expt n exp)
  (define (odd? x)
    (= (remainder x 2) 1))
  (define (square x) (* x x))
  (define (expt-iter res n exp)
    (cond ((= exp 0) res)
          ((odd? exp) (expt-iter (* res n) n (- exp 1)))
          (else (expt-iter res (square n) (/ exp 2)))
    )
  )
  (expt-iter 1 n exp)
)

;type checkers
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;common selectors
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;functions for making expressions
(define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2) 
          ((=number? a2 0) a1) 
          ((and (number? a1) (number? a2)) (+ a1 a2)) 
          (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m1 1) 1)
        ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list '** m1 m2))))

;derivation package
(define (install-derivation-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))

  (define (deriv-product operands var)
    (make-sum
     (make-product (car operands)
                   (deriv (cadr operands) var))
     (make-product (deriv (car operands) var)
                   (cadr operands))))

  (define (deriv-exponent operands var)
    (make-product
          (make-product
           (cadr operands)
           (make-exponentiation (car operands)
                                (make-sum (cadr operands) -1)))
          (deriv (car operands) var)))
  ;;sys interface
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent))

(install-derivation-package)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ x (* 2 x)) 'x)
(deriv '(** x 10) 'x)
(deriv '(** (* x x) 10) 'x)