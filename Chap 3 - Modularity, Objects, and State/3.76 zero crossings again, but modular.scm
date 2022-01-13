(define (avg a b)
  (/ (+ a b) 2))

(define (smooth stream)
  (cons-stream
   (avg (stream-ref stream 0) (stream-ref stream 1))
   (smooth (stream-cdr stream))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define smooth-zero-crossings (make-zero-crossings (smooth (cons-stream 0 sense-data) 0)))