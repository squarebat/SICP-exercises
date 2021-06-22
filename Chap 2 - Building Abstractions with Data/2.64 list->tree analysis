#lang sicp
#|
partial tree works as follows -
base case: a partial tree with 0 elements is a null tree

The tree is constructed by constructing a balanced left-branch and right-branch
and merging them together with root. This gives us
Left Branch Size: (n - 1)/2
Root: 1 element
Right Branch Size: n - (Left Branch Size + 1(root))

Once we have both left branch and right branch, we merge them together at root
with the make-tree procedure

This gives us the growth
T(n) = T(n/2) + O(1) {make-tree takes constant time}
Which evaluates to theta(n)
|#
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))