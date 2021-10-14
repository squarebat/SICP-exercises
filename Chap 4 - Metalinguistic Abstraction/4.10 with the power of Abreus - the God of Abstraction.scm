;in the eval procedure we basically just have a checks to determine type of expression.
;All of which are handled by separate procedures
;If we modify how those procedures evaluate expressions, then voila! We got ourselves a new language
;For example let's change the syntax of an assignment by making it look like this
(new-var = 10)
;we just need a change in below procs
(define (assignment? exp)
  (tagged-exp? exp '=))
(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))

;by using tagged-exp? we can change the syntax of a whole lot of other expressions
(define (tagged-exp? exp quote)
  (and (not (null? (cdr exp)))
       (eq? (cadr exp) quote)))
