;letrec is of the form
(letrec ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)
;to be converted to the form
(let ((<var1> '*unassigned*)
      (<var2> '*unassigned*)
      (...))
  (let ((a <exp1>)
        (b <exp2>)
        (...))
    (set! u a)
    (set! v b)
  <body>))
;- we use the form from ex 4.18 because this makes the bindings truly simultaneous
;- we expect it to throw an error for evaluations like the one in 4.18
;- in essence, cyclic dependencies will only be allowed by letrec if they are wrapped
;by a lambda

(define (letrec? exp) (tagged-list? exp letrec))
(define (letrec-inits exp) (cadr exp))
(define (letrec-vars exp) (map car (letrec-inits exp)))
(define (letrec-bindings exp) (map cadr (letrec-inits exp)))
(define (letrec-body exp) (caddr exp))
(define (get-symbols exp) (map
                           (lambda (x) (next-symbol interpreter-symbol-list))
                           (letrec-inits exp)))

;where interpreter-symbol-list is an infinite stream of randomly generated symbols
(define (letrec->let exp)
  (let ((intermediate-symbols (get-symbols exp)))
    (make-let (map (lambda (var) (list var '*unassigned*)) (letrec-vars exp))
              (list (make-let (map list intermediate-symbols (letrec-bindings exp))
                              (map (lambda (var value) (list 'set! var value)) (letrec-vars exp) intermediate-symbols))
                    (letrec-body exp)))))


;=============Part 2
#|
Louis is not completely wrong, we can use let instead of defines, the execution of the body
will just happen in a new environment as discussed in exercise 4.17

However let cannot be used in place of letrec, it can't handle recursion. You can't refer
to a variable while defining it in the let scope because of how it is encoded.
|#

;below is an excellent explanation on difference between let and letrec by leafac on
;http://community.schemewiki.org/?sicp-ex-4.20

;=============NOT MINE===================
;; The lambda in `let' is evaluated in the context of the enclosing environment,
;; in which the bindings of the lambda itself are not in place.

;; The trick of encoding `letrec' is that we first establish the bindings, and
;; then define the lambdas in an environment where the bindings are there, so
;; the recursive call can succeed.

;; The following snippets illustrate the difference:

(let ((fact <fact-body>))
  <let-body>)

;; is encoded by

((lambda (fact)
   <let-body>)
 <fact-body>)

;; such that `<fact-body>' can't refer to `fact'. While:

(letrec ((fact <fact-body>))
  <let-body>)

;; is encoded by

((lambda (fact)
   (set! fact <fact-body>)
    <fact-body>)
 '*unassigned*)

;; note that in the context of `<fact-body>', the variable `fact' itself is
;; bound.
