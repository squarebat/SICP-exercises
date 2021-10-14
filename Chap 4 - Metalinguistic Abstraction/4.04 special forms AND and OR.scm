;install procs
(put 'op 'and eval-and)
(put 'op 'or eval-or)

;eval-and and eval-or as special forms
(define (eval-and exp env)
  (let ((val (eval (first-exp exps) env)))
    (cond ((last-exp? exps) val)
          (val (eval-and (rest-exps exps) env))
          (else val))))

(define (eval-or exp env)
  (let ((val (eval (first-exp exps) env)))
    (cond ((last-exp? exps) val)
          (val val)
          (else (eval-or (rest-exps exps) env)))))

;and and or as derivative forms
(define (and->if exp)
  (expand-and-clauses (clauses exp)))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (let ((first (car clauses))
            (rest (cdr clauses)))
            (make-if (first-clause clauses)
                     (expand-and-clauses rest)
                     'false))))
(define (or->if exp)
  (expand-or-clauses (clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if (first-clause clauses)
                 'true
                 (expand-or-clauses rest)))))
