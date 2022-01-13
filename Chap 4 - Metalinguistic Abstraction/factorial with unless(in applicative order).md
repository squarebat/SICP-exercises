## Applicative Order Vs Normal Order Evaluation

```SCM
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
```

Tracing the above proc for (factorial 5) we get:

### In Applicative Order
While evaluating facctorial 5, a call to `unless` is made. Before unless can execute, all 3 expressions passed to unless are evaluated. That is:

- (= 5 1) => false
- (* 5 (factorial (- 5 1)))
- 1

Except the third expression will never be reached, because each call to factorial will call unless, which in turn will evaluate (factorial (- n 1)). Thus, even on reaching the base case 1, the recursion will continue with (factorial 0) (factorial -1) and so on infinitely and 1 will never be returned.

### In normal order

- In normal order evaluation, the call to unless will only evaluate expressions when needed. Thus the body of unless executes and the call to speacial form if evenually returns 1 on reaching the base case (= n 1).

### Conclusion

Lazy evaluation OP



