#lang sicp
#|
returns element at index-th position in row-th row of pascal's triangle
if index is invalid returns -1
row and index are 1-indexed
|#
(define (pascals-triangle row index)
  (cond ((> index row) -1)
        ((or (= index 1) (= index row)) 1)
        ((+ (pascals-triangle  (- row 1) (- index 1)) (pascals-triangle  (- row 1) index)))
  )
)    

(pascals-triangle 1 1)
(pascals-triangle 2 1)
(pascals-triangle 2 2)
(pascals-triangle 3 1)
(pascals-triangle 3 2)
(pascals-triangle 3 3)
(pascals-triangle 4 1)
(pascals-triangle 4 2)
(pascals-triangle 4 3)
(pascals-triangle 4 4)