#lang sicp
;---- QUEUE IMPLEMENTATION ----
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;---- PRINT QUEUE ----
; The reason last element gets printed twice is because it is and element of the the list pointed
; to by front-ptr, and it is pointed to by the last ptr as well.
; to print the queue alone, we need to process the list pointed to by front-ptr

(define (for-each items proc)
  (if (not (null? items))
      ((lambda ()
        (proc (car items))
        (for-each (cdr items) proc)))))

(define (print-queue queue)
  (newline)
  (for-each (front-ptr queue) (lambda (item)
                                (display item)
                                (display " ")))
  (newline))

; Test

(define q (make-queue))
(insert-queue! q 1)
(insert-queue! q 2)
(insert-queue! q 3)
(insert-queue! q 4)
(print-queue q)