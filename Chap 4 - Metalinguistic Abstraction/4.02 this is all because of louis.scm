;Louis's change doesn't work because now a call to (define) will be accepted by the application clause and apply will be called on it. However, it is neither a primitive
;procedure nor a compound one and hence the apply will throw an error

;on to part b, let's rewrite that evaluator for louis

;now applications will be recognized by the call tag so procedure-application? will be modified as
(define (application? exp) (tagged-list? exp 'call))
;get rid of call tag by modifying operator and operands procs
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
