#lang sicp
(define (fringe items)  
  (define (iter items result) 
    (cond ((null? items) 
           result) 
          ((pair? items) 
           (iter (car items) 
                        (iter (cdr items) result))) 
          (else (cons items result)))) 
  (iter items nil)
) 
  
(fringe (list (list 1 2) (list 3 4 (list 5 (list 6))))) 
