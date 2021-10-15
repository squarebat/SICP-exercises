;this
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;converts to this
(define solve
  (lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y)))

;the above proc will not work. While evaluating value of var b, the value of y is not defined yet.
;the form as used by the text will work because it defines y before it is used by stream map
;pretty sure louis came up with this new form for some unnecessary reason
