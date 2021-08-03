#lang sicp
(#%require sicp-pict)

(define (split direction1 direction2)
  (define (splitter painter n)
    (if (= n 0)
      painter
      (let ((smaller (splitter painter (- n 1))))
        (direction1 painter (direction2 smaller smaller)))))
  splitter)

(define up-split (split below beside))
(define right-split (split beside below))

(paint (up-split einstein 3))
(paint (right-split einstein 3))