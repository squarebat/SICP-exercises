(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (if (stream-null? integrand)
                       the-empty-stream
                       (integral (stream-cdr integrand)
                                 (+ (* dt (stream-car integrand))
                                    initial-value)
                                 dt)))))