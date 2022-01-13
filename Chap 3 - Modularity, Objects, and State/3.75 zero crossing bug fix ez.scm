;what alyssa needs is every value averaged with the last value. Loose reasoner's version however,
;averages a value with the last average, causing the value to remember all signal values from the beginning of stream. 
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
