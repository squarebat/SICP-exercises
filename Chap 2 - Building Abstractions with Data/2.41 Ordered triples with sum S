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

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list k j i))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;sum of triples 
(define (sum-equals-s? triple s)
  (= s (+ (car triple) (cadr triple) (caddr triple))))

(define (triples-with-sum-s n s)
  (filter (lambda (triple) (sum-equals-s? triple s)) (ordered-triples n)))

;test
(ordered-triples 5)
(triples-with-sum-s 5 8)
(triples-with-sum-s 5 9)
(triples-with-sum-s 5 10)
(triples-with-sum-s 5 11)
(triples-with-sum-s 5 12)
