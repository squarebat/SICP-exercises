(define (reciprocal-series series)
  (define reciprocal-stream (cons-stream 1 (mul-series reciprocal-stream (scale-stream -1 (stream-cdr series)))))
  reciprocal-stream)

(define (div-series num den)
  (if (= (stream-car den) 0)
      (error "DIVIDE BY ZERO")
      (mul-series num (reciprocal-series den))))

(define tan-series (div-series sine-series cosine-series)) 