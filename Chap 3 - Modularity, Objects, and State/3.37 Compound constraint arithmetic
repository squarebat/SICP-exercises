#lang sicp
(define (c+ connector-a connector-b)
  (let ((sum (make-connector)))
    (adder connector-a connector-b sum)
    sum))

(define (c* connector-a connector-b)
  (let ((product (make-connector)))
    (multiplier connector-a connector-b product)
    product))

(define (c/ connector-a connector-b)
  (let ((quotient (make-connector)))
    (multiplier quotient connector-b connector-a)
    quotient))

(define (cv value)
  (let ((const (make-connector)))
    (constant value const)
    const))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))