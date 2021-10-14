;e.g of a for loop
(for (i 0) (< i 10) (+ i 1)
     sequence-of-exps)

;e.g of while loop
(while (< var 10) sequence)

;e.g of do while
(do ((exp1)
     (exp2)
     (...)
     (expn))
    until (< var 10))

;convert for to the following form
(let for ((i 0)
          (count 10)
          (body (sequence->exp sequence-of-exps)))
    (if (< i count)
        (begin
          (body)
          (for (+ i 1) 10 body))))

;evaluator for for loop
;syntax checking has not been implemented for simplicity (totally not because I don't want to)
(define (for? exp) (tagged-list? exp 'for))
(define (for-body exp) (cddddr exp))
(define (for-iter exp) (cadr exp))
(define (for-count exp) (caddr (caddr exp)))
(define (for-predicate exp) (caddr exp))
(define (for-change-iter exp) (cadddr exp))
(define (for->named-let exp)
  (make-named-let 'for
                  (list
                   (for-iter exp)
                   (cons 'count (for-count exp))
                   (cons 'body (sequence->exp (for-body exp))))
                  (make-if (for-predicate exp)
                           (make-begin '(body)
                                       '(for (for-change-iter exp)
                                             count
                                             body))
                           'done)))
