#|
variables may have different values in enclosing environment
make-unbound! should ideally make variables unbound in the local scope
That is, just remove the variable from the first frame it is bound in.
This allows us to use that variable name in the local scope again, while
preserving value in the global scope.
We could however implement both global and local scope removals by supplying an
extra argument to specify scope, however YIKES, modifying the global scope is dangerous
100% would not recommend (or implement).
|#

;We'll be using the abstracy form of env-loop from ex 4.12
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

(define (make-unbound! var env)
  (env-lookup env
              (lambda (vals) (set! vals (cdr vals)))
              (lambda (env) (error "Variable already unbound" var))
              (lambda () (error "EMPTY Environment -- UNBIND" var))))
