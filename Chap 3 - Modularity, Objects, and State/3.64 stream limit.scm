;note: stream must be infinite
(define (stream-limit stream tolerance)
  (let ((first (stream-ref stream 0))
        (second (stream-ref stream 1)))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))
