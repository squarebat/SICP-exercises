#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;map 
(define (map p sequence)
  (accumulate (lambda (current-ele accum-rest) (cons (p current-ele) accum-rest)) nil sequence))

;append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;sequence-length
(define (length sequence)
  (accumulate (lambda (current-ele accum-rest) (+ 1 accum-rest)) 0 sequence))

(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))

(map (lambda (x) (* x x)) list1)
(append list1 list2)
(length list1)
