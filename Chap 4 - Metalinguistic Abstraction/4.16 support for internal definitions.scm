(define *unassigned* '*unassigned*)
;signal error in lookup variable value
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) *unassigned*)
                 (error "No value assigned to variable yet" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;scan defines in procedure body and convert to let form
;not that the 'let' used by the proc is the one provided by the underlying scheme
;the call to let is a part of the implemenation language
;for using the let in our implemented language, we use make-let
(define (scan-out-defines procedure-body)
  (let ((var-bindings '()))
    (define (get-define-objects! procedure-body)
      (if (not (null? procedure-body))
          (begin
            (if (definition? (car procedure-body))
                (begin
                  (set! var-bindings (cons
                                      (cons (cadr (car procedure-body))
                                            (caddr (car procedure-body)))
                                      var-bindings))
                  (set! procedure-body (cdr procedure-body))))
            (get-define-objects (cdr procedure-body)))))
    (get-define-objects! procedure-body)
    (make-let var-bindings procedure-body)))

;it would be better to install scan-out-defines in the make-procedure proc, because there, it gets evaluated only once
;if we install it in procedure body, it'll be evaluated everytime it's called
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
