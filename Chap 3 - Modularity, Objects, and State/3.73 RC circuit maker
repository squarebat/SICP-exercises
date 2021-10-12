(define (RC-circuit-maker R C dt)
  (define (get-voltage-output input-current-stream initial-voltage)
    (add-streams
     (scale-stream input-current-stream R)
     (integral (scale-stream input-current-stream (/ 1 C))
               initial-voltage dt)))
  get-voltage-output)

