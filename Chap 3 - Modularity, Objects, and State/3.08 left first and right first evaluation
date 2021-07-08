#lang scheme
(define f
  (let ((var 1))
    (lambda (arg)
      (cond ((not (= var 0)) (set! var arg))
            ((eq? arg 'reset) (set! var 1)))
      var)))

;we see that evaluation occurs left to right by interpreter.
(+ (f 0) (f 1))
;reset. Note that reset is only required to be able to simulate right first evaluation.
(f 'reset)
;Simulate right first evaluation
(+ (f 1) (f 0))
