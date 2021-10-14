;we'll need to modify the expand clauses procedure
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define => 'to-recipient-operator)

(define (is-test-predicate? exp)
  (eq? (cadr exp) =>))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (let ((simplified-first
              (if (is-test-predicate? first)
                  (cons (cond-predicate first)
                        (lambda ()
                          ((cadr (cond-actions first)) (cond-predicate first))))
                  first)))
          (if (cond-else-clause? simplified-first)
              (if (null? rest)
                  (sequence->exp (cond-actions simplified-first))
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses))
              (make-if (cond-predicate simplified-first)
                       (sequence->exp (cond-actions simplified-first))
                       (expand-clauses rest)))))))
