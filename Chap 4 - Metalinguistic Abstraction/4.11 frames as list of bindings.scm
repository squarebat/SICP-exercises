;this is the right way to do it tbh
;Why? because now i have to look through only one list to get my variable instead of two
(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cadr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) frame)))

;change how lookup works
(define (lookup-variable-value var env)
     (define (env-loop env)
         (if (eq? env the-empty-environment)
             (error "Unbound variable" var)
             (let ((ret (assoc var (first-frame env))))
                 (if ret
                     (cdr ret)
                     (env-loop (enclosing-environment env))))))
     (env-loop env))

 (define (set-variable-value! var val env)
     (define (env-loop env)
         (if (eq? env the-empty-environment)
             (error "Unbound variables -- SET!" var)
             (let ((ret (assoc var (first-frame env))))
                 (if ret
                     (set-cdr! ret val)
                     (env-loop (enclosing-environment env))))))
     (env-loop env))

 (define (define-variable! var val env)
     (let* ((frame (first-frame env))
            (ret (assoc var frame)))
         (if ret
             (set-cdr! ret val)
             (add-binding-to-frame! var val frame))))

;one can also use the 1D tables as implemented in Chapter 3
;Why would one wanna use it? Cause table lookup is optimized, we implemnted storing tables as binary search trees
