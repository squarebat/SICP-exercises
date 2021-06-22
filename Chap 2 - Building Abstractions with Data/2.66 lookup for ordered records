#lang sicp
(define (key record) (car record))
(define (value record) (cadr record))

(define (adjoin-record x set)
  (cond ((null? set) (list x))
        ((< (key x) (key (car set))) (cons x set))
        (else (cons (car set) (adjoin-record x (cdr set))))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records))) (car set-of-records))
        ((< given-key (key (car set-of-records))) false)
        (else (lookup given-key (cdr set-of-records)))))

(lookup 2 (list '(1 2) '(2 3) '(3 4)))