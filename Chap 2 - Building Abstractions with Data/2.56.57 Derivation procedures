#lang sicp
;type checkers
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;Generic proc
(define (eval-exp proc a b)
  (if (and (number? a) (number? b))
      (begin (cond ((eq? proc '+) (+ a b))
                   ((eq? proc '*) (* a b))))
      (list proc a b)))
;Sum
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (if (null? (cdddr s))
                       (caddr s)
                       (make-sum-list (cddr s))))

(define (make-sum-list l)
  (if (= (length l) 2) 
      (eval-exp '+ (car l) (cadr l)) 
      (make-sum (car l) (make-sum-list (cdr l))))) 

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (make-sum-list (list a1 a2))))) 

;Product
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (make-product-list (cddr p))))

(define (make-product-list l) 
  (if (= (length l) 2) 
      (eval-exp '* (car l) (cadr l)) 
      (make-product (car l) (make-product-list (cdr l))))) 

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-product-list (list m1 m2))))) 

;Exponentiation
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

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (make-exponentiation m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m1 1) 1)
        ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list '** m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp)
                                (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;test 
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ x (* 2 x) (* x x)) 'x)
(deriv '(** x 10) 'x)
(deriv '(** (* x x) 10) 'x)