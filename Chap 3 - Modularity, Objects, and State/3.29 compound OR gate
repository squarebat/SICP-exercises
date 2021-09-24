#lang sicp
#|
To make OR gate from AND and NOT gates - 
both inputs are inverted, the ouputs are input to AND. The output
of AND is again inverted. 
delay of or gate with respect to and and or gates used is equal to
inverter-delay + and-gate-delay + inverter-delay
|#
(define (compound-or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (and-out (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 and-out)
    (inverter and-out output))
  'ok)

