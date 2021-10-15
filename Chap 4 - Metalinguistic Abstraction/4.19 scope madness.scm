#|
I tend to agree with ben bit diddle here, the result should be obtained sequentially this can
be done while using the global value of 'a' for evaluating value of 'b' if it is
unassigned in the local scope. It expression would be converted to the below form as follows
|#
;this
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
;to this
(let ((a 1))
  (let ((f '*unassigned))
    (set! f (lambda (x)
              (let ((b '*unassigned)
                    (a '*unassigned))
                (set! b (+ 1 x)) ;after a is eval-ed
                (set! a 5)
                (+ a b))))
    (f 10)))

(Alyssa)
#|
However ben's is an imperative approach, it will introduce all the bugs akin to imperative
languages. In that sense, Alyssa's approach would be the right way to do it, by throwing an erroe
forcing the programmer to write better code.
|#

(Eva)
;implementing what eva wants is a bit messy, but here' how it would work
#|
- while evaluating b, we find that the value of a is unassigned, so we look through
the rest of local scope for the value of variable a, evaluate that and later evaluate b
- this does raise a few questions though:
    - how far should we evaluate the value of 'a' before using it for evaluating 'b'.
      Should it be right after 'a' is defined or, after any set! expressions involving 'a'
      have been evaluated already.
    - Do we really want to use simultaneous scope in a way the value of a variable is used
      before it is defined? Not only is this bug prone, it also makes lookup a lot more
      expensive.
|#

;a better approach acc to xdavidliu's solution on http://community.schemewiki.org/?sicp-ex-4.19
#|
- We first take out all the function definitions and put them at the top,
since their bodies are delayed and hence will not pose any issues whatsoever.
- Then, we take the list of the non-function definitions, generate a list of the matching
dependent variables in each body, and repeatedly take out all non-function definitions
whose bodies are independent of any remaining non-function variables.
- This is slower than implementing topological sort on a directed graph, but it works fine,
and has the added bonus of being able to naturally check for cycles.
|#
