#lang sicp
;the result stream is the digits of the rational value that will be produces by dividing num with den in base radix.
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))