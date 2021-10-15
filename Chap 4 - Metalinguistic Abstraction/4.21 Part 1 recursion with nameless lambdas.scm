((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;lets call the three lambdas factorial, L0 and L1 respectively (for simplicity)
;the above call will then look like
(factorial 10)
;L1 is passed as argument to L0, the body of fact looks like
((L0 L1) 10)
;which becomes
(L1 L1 10)
;L1 is indeed a lambda that takes 2 arguments, one of them is L1 itself
;the substitution looks like-
(if (= 10 1)
    1
    (* 10 (L1 L1 (- 10 1))))

;(L1 L1 (- k 1)) can be substituted as
(if (= 10 1)
    1
    (* 10 (factorial (- 10 1))))

;the substitution shows that the body of factorial will actually evaluate factorial
;fib can be defined similarly (continued in 4.21 Part 2)
