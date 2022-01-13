;unless as a special form
;add below condition to core evaluator proc
((unless? exp) (eval (unless->if exp) env))
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))
#|
unless as a procedure can help with applying masks and mixing data using maps
For example, you have two lists of  ones and twos, and a mask list (#f #t #f #t ...)
calling the following proc -
    (map unless mask ones twos)
will return the list (1 2 1 2 1 2 ...)

If we didn't have unless available as a procedure, we wouldn't be able to use map
and would have to write a proc from scratch. This doesn't allow us to use a useful
abstraction.

Gee I can't think of other good examples where unless would be useful as a proc ATM,
but as wiki says <this list is incomplete, you can help by adding to it!>
|#
