#lang sicp
; Semaphore implementation in terms of test-and-set! (assumed atomic)
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (test-and-set-semaphore! avail cell) ;this should run atomically as well
  (if (= avail 0)
      true
      (begin
        (set! avail (- avail 1))
        (if (= avail 0)
          (test-and-set! cell)))))

(define (make-semaphore n)
  (let ((avail n)
        (cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set-semaphore! avail cell)
                 (the-semaphore 'acquire))) 
            ((eq? m 'release)
             (begin
               (clear! cell)
               (set! avail (+ avail 1))) ; this sequence ensures that cell is set to false before slot is made available
             )))
    the-semaphore))

;Semaphore implementation in terms of mutex
(define (make-mutex)
  (define (clear! cell)
    (set-car! cell false))
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))


(define (make-semaphore n)
  (let ((avail n)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (= avail 0)
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))
                 (begin
                   (set! avail (- avail 1))
                   (mutex 'release)))) 
            ((eq? m 'release)
             (begin
               (mutex 'acquire)
               (set! avail (+ avail 1))
               (mutex 'release))
             )))
    the-semaphore))
