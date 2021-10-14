;it is pretty iffy yes, it could be made simpler if we were to remove support for the original implemataion of let
(define (let? exp) (tagged-list exp 'let))

(define (let-body exp)
  (if (symbol? (cadr exp))
      (cadddr exp)
      (caddr exp))

(define (let-name exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      false))

(define (let-variable-names exp)
  (if (list? (cadr exp))
      (map car (cadr exp))
      (if (list? (caddr exp))
          (map car (caddr exp))
          (error "INVALID SYNTAX - let"))))

(define (let-variable-names exp)
  (if (list? (cadr exp))
      (map cadr (cadr exp))
      (if (list? (caddr exp))
          (map cadr (caddr exp))
          (error "INVALID SYNTAX - let"))))

#|
Named let can be derived from the following let form
(let ((var1 bind1)
      (var2 bind2)
      (...))
  (let ((<name-of-let> (lambda (var1 var2 ...) named-let-body)))
    named-let-body)
))
|#

(define (let->combination exp)
  (if (let-name exp)
      '((make-lambda (let-variable-names exp)
                     (make-let (list (cons (let-name exp)
                                           (make-lambda (let-variable-names exp)
                                                        (list (let-body exp)))))))
        (let-variable-values exp))
      '((make-lambda (let-variable-names exp) (list (let-body exp)))
        (let-variable-values exp))))
