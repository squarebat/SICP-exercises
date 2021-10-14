;oh boy this was tricky
(define (env-lookup env on-found-op on-null-list-op on-end-env-err)
  (define (scan vars vals)
    (cond ((null? vars)
           (on-null-list-op env)
          ((eq? var (car vars))
           (on-found-op vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (on-end-env-err)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame))))))

(define (lookup-variable-value var env)
  (env-lookup env
              (lambda (vals) (car vals))
              (lambda (env) (lookup-variable-value var (enclosing-environment env)))
              (lambda () (error "Unbound variable" var))))

(define (set-variable-value! var env)
  (env-lookup env
              (lambda (vals) (set-car! vals var))
              (lambda (env) (set-variable-value! var (enclosing-environment env)))
              (lambda () (error "Unbound variable -- SET" var))))

(define (define-variable! var val env)
  (env-lookup env
              (lambda (vals) (set-car! vals val))
              (lambda (env) (add-binding-to-frame var val (first-frame env)))
              (lambda () (error "EMPTY Environment -- DEFINE" var))))
