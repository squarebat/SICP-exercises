#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-m)
           (map (lambda (col)
                  (dot-product row-m col))
                cols))
         m)))

(define seqs0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define seqs1 (list (list 1 1 1) (list 1 1 1) (list 1 1 1) (list 1 1 1)))
(define seqs2 (list (list 1 1) (list 1 1) (list 1 1)))

(matrix-*-vector seqs0 (list 1 1 1))
(transpose seqs0)
(matrix-*-matrix seqs1 seqs2)

