#lang sicp
;both methods will give same result
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;tests on figure 2.16
(define tree1 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())))
(define tree2 (make-tree 9 '() (make-tree 11 '() '())))
(define tree3 (make-tree 7 tree1 tree2))
(tree->list-1 tree3)
(tree->list-2 tree3)

(define tree4 (make-tree 7 (make-tree 5 '() '()) tree2))
(define tree5 (make-tree 3 (make-tree 1 '() '()) tree4))
(tree->list-1 tree5)
(tree->list-2 tree5)

(define tree6 (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))
(define tree7 (make-tree 3 (make-tree 1 '() '()) '()))
(define tree8 (make-tree 5 tree7 tree6))
(tree->list-1 tree8)
(tree->list-2 tree8)