(define (invert-unit-series series)
  (define invert-stream (cons-stream 1 (mul-series invert-stream (scale-stream -1 (stream-cdr series)))))
  invert-stream)
