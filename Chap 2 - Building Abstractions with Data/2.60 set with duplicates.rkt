#lang sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (remove-duplicates set)
  (cond ((null? set) set)
        ((element-of-set? (car set) (cdr set)) (remove-duplicates (cdr set)))
        (else (cons (car set) (remove-duplicates (cdr set))))))

(define (intersection-set set1 set2)
  (define (intersection set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)        
           (cons (car set1)
                 (intersection (cdr set1) set2)))
          (else (intersection (cdr set1) set2))))
  (remove-duplicates (intersection set1 set2)))

(define (union-set set1 set2)
  (define (union set1 set2)
    (cond ((null? set1) set2)
          ((element-of-set? (car set1) set2)        
           (union-set (cdr set1) set2))
          (else (union-set (cdr set1) (cons (car set1) set2)))))
  (remove-duplicates (union set1 set2)))

(intersection-set (list 1 2 3 3 3 ) (list 2 3 4 4 4 4))
(union-set (list 1 2 3 3 3 ) (list 2 3 4 4 4 4))