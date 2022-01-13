#lang sicp
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (cond ((null? sub-tree) nil)
               ((not (pair? sub-tree)) (proc sub-tree))
               (else (tree-map proc sub-tree))))
       tree))

;define square-tree using tree-map
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

;define scale-tree using tree map
(define (scale-tree tree factor) (tree-map (lambda (x) (* x factor)) tree))

;test
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(scale-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))
 10)