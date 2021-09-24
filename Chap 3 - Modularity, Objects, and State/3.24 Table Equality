#lang sicp
;tolerance equality implementation
(define (is-equal? a b tolerance)
  (if (and (number? a) (number? b))
      (< (abs (- a b)) tolerance)
      (equal? a b)))
;table implementation
(define (make-table tolerance-val)
  (let ((local-table (list '*table*))
        (tolerance tolerance-val))
    (define (assoc key records)
      (cond ((null? records) false)
            ((is-equal? key (caar records) tolerance) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table (make-table 0.01))
(define get (table 'lookup-proc))
(define put (table 'insert-proc!))

(put 5 5 'data1)
(put 6 7 'data2)
(put 'a 7 'data3)
(put 'a 'b 'data4)
(get 5.001 5.009)
(get 6.001 7)
(get 6.1 7)
(get 'a 7.001)
(get 'a 'b)
