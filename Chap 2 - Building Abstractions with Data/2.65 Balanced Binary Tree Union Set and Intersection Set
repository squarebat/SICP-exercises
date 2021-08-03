#lang sicp
;Tree representation
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

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

(define (balance-tree tree)
  (list->tree (tree->list tree)))

;Set representation as tree
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (balance-tree
   (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))))))

(define (union-set set1 set2)
   (cond ((null? set1) set2)
         ((element-of-set? (entry set1) set2)        
          (union-set (union-set (left-branch set1) (right-branch set1))
                     set2))
         (else (adjoin-set (entry set1)
                           (union-set (union-set (left-branch set1) (right-branch set1))
                                      set2)))))

(define (intersection-set set1 set2)
   (cond ((or (null? set1) (null? set2)) '())
         ((element-of-set? (entry set1) set2)        
          (make-tree (entry set1)
                     (intersection-set (left-branch set1) set2)
                     (intersection-set (right-branch set1) set2)))
         (else (union-set
                (intersection-set (left-branch set1) set2)
                (intersection-set (right-branch set1) set2)))))

;tests on figure 2.16
(define tree1 (make-tree 3 (make-tree 1 '() '()) (make-tree 6 '() '())))
(define tree2 (make-tree 9 '() (make-tree 11 '() '())))
(define tree3 (make-tree 8 tree1 tree2))

(define tree4 (make-tree 7 (make-tree 5 '() '()) tree2))
(define tree5 (make-tree 3 (make-tree 1 '() '()) tree4))

(define tree6 (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))
(define tree7 (make-tree 3 (make-tree 1 '() '()) '()))
(define tree8 (make-tree 5 tree7 tree6))

(tree->list (union-set tree3 tree5))
(tree->list (intersection-set tree3 tree5))