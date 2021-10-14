(define operation-table make-table)
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

(put 'op 'quote text-of-quotation)
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (exp env)
                   (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'op 'begin (lambda (exp env) (eval-sequence (begin-sequence exp) env)))
(put 'op 'cond  (lambda (exp env) (eval (cond-if exp) env)))

(define (type exp) (car exp))
(define (eval exp env)
  (let ((evaluator (get 'op (type exp))))
    (cond
     ((self-evaluating? exp) exp)
     ((variable? exp) (lookup-variable-value exp env))
     (evaluator (evaluator exp env))
     ((application? exp)
      (apply (eval (operator exp) env)
             (list-of-values (operands exp) env)))
     (else
      (error "Unknown expression type -- EVAL" exp)))))