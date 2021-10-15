(a)
;the let expression is basically just a call to lambda behind the scenes.
;every call to lambda extends the environment and hence the extra frame
(b)
;the extra let can't affect the working of the proc, because the entire proc
;is just scoped to a new environment, and doesn't affect the environments enclosing it
(c)
;we can remove the extra frame by doing away with the let expression
;instead we can scan all defines and define them as *unassigned* at the top of the body as follows-
(lambda <vars>
  (define u '*unassigned)
  (define v '*unassigned)
  <e0>
  (define u <e1>)
  <e2>
  <e3>
  (define v <e4>)
  <e5>
  <e6>)
;note that we can't just move all the defines with their assigned value at the top
;since it might allow the proc to use variable values before they were actually defined
