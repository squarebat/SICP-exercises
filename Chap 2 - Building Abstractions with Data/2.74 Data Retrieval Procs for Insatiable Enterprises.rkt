#lang scheme
;for each
(define (for-each proc items)
  (if (not (null? items))
      ((lambda ()
        (proc (car items))
        (for-each proc (cdr items))))))

;table for data direction
(define *op-table* (make-hash))

(define (put proc-name division proc)
  (hash-set! *op-table* (list proc-name division) proc))

(define (get proc-name division)
  (hash-ref *op-table* (list proc-name division) #f))

(define (open-records division)
  ;open record file of a division
  ;inside a division each employee is keyed by name
  ;further, constructors and selectors can be inplemented for fields like address, salary etc
  )

(define (install-division-package)
  ;representation of employee in a specific divsion type
  ;implementation of procs get-record and set-record
  ;attach tag and put procs in proc table
  )

(define (get-record employee division)
  ((get 'get-record division) employee))

(define (get-salary employee division)
  ((get 'get-salary division) employee))

(define (find-employee-record employee divisions)
  (for-each (lambda (division)
              (let ((record ((get 'get-record division) employee)))
                (if (not (null? record))
                    record)) divisions)))



