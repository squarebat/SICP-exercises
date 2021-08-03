(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound intr)
  (car intr))
(define (upper-bound intr)
  (cdr intr))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent-width)
  (let ((tolerance (* center (/ percent-width 100))))
    (make-center-width center tolerance))
)

(define (percent i)
  (* (/ (width i) (center i)) 100))