#lang sicp
#|

subsets is a recursive function to find all subsets of a set for a set (powerset) represented by a list.
It works as follows:

1: If set has no items (empty set), the powerset is a set containing an empty set.
2: For a set with One or more items, powerset can be formed as follows:
  i - Remove first element (x) from the set and find powerset of the resulting set. 
  ii - Insert (x) to all sets belonging to the powerset from step i
  iii - The powerset is the set containing all sets formed in step i and ii 

|#

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (list) (cons (car s) list)) rest)))))

(subsets (list 1 2 3))