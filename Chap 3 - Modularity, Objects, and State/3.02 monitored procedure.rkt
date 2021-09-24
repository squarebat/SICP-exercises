#lang scheme
(define (make-monitored proc)
  (let ((calls 0))
    (lambda (message)
      (cond ((eq? message 'how-many-calls?) calls)
            ((eq? message 'reset-count) (set! calls 0))
            (else (begin
                    (set! calls (+ calls 1))
                    (proc message)))))))

(define m-square (make-monitored (lambda (x) (* x x))))
(m-square 3)
(m-square 5)
(m-square 'how-many-calls?)
(m-square 3)
(m-square 5)
(m-square 'how-many-calls?)
(m-square 'reset-count)
(m-square 'how-many-calls?)

