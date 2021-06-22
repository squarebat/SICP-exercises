#lang sicp
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;representation of board
(define empty-board nil)

;an nxn board is represented as a list where ith element is the row num at which a queen is placed in the (n-i+1)th column
;i.e positions are stored in reverse order, however it'll still be a valid solution

(define (adjoin-position row col board)
  (cons row board))

(define (same-row? pos1 pos2)
  (= pos1 pos2))

(define (same-diagonal? pos1 pos2 distance)
  (or
   (= pos2 (+ pos1 distance))
   (= pos2 (- pos1 distance))))

(define (safe? k positions)
  (let ((new-row (car positions)))
    (define (iter col rest-positions)
    (cond ((= col k) #t)
          ((or
            (same-row? new-row (car rest-positions))
            (same-diagonal? new-row (car rest-positions) (- col 0))) #f)
          (else (iter (+ col 1) (cdr rest-positions)))))
  (iter 1 (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))
