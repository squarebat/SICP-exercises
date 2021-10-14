(define (list-of-values-l-to-r exps env)
    (if (no-operands? exps)
        '()
        (let ((evaluated (eval (first-operand exps) env)))
        (cons evaluated
              (list-of-values (rest-operands exps) env)))))


(define (list-of-values-r-to-l exps env)
    (if (no-operands? exps)
        '()
        (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)                                                                                                                                  
              rest))))
