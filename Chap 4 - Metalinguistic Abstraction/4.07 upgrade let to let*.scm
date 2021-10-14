;very much like cond->if
(define (let*->nested-lets exp)
  (expand-let (vars exp) (body exp))) ;vars is (cadr exp) body is (caddr exp)

(define (make-let vars body)
  (list 'let vars body))

(define (expand-let vars body)
  (if (last-var? vars)
      (make-let vars body)                          ; no else clause
      (let ((first (car vars))
            (rest (cadr vars)))
         (make-let (list first)
                   (expand-let rest body)))))

;install let* in eval
(put 'op 'let* (lambda (exp env)
                 (eval (let*->nested-lets exp) env)))
