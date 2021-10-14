;transform this
(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

;to this
((lambda (<var1> ... <varn>)
   <body>)
 <exp1>

 <expn>)

;EXERCISE STARTS HERE
(define (let? exp) (tagged-list exp 'let))
(define (let-body exp) (caddr exp))
(define (let-variable-names exp) (map car (cadr exp)))
(define (let-variable-values exp) (map cadr (cadr exp)))
(define (let->combination exp)
  '((make-lambda let-variable-names let-body)
    let-variable-values))

;install this to eval with
(put 'op 'let
     (lambda (exp env)
       (eval (let->combination exp) env)))

;note to self - data directed programming ftw
