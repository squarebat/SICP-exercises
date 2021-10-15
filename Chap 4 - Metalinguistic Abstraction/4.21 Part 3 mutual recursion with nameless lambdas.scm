;even odd mutual recursion with lambdas
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   ;even?
   (lambda (ev? od? n)
     (if (= n 0) 'true (od? ev? od? (- n 1))))
   ;odd?
   (lambda (ev? od? n)
     (if (= n 0) 'false (ev? ev? od? (- n 1))))))

(newline)
(display (f 10))
(newline)
(display (f 11))
(newline)
(display (f 12))
