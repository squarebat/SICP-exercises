(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-stream ones integers)))
;first approximation series 
(define ln2-summands (cons-stream 1
                          (stream-map /
                                      (scale-stream ln2 -1)
                                      integers)))
(define ln2 (partial-sums ln2-summands))

;accelerated approximation
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define ln2-accel (euler-transform ln2))

;third sequence
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define ln2-super-accel (accelerated-sequence euler-transform ln2))