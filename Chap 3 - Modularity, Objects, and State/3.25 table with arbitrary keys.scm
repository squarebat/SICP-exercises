#lang sicp
#|
we store the table as a list of key value pairs, where each pair
consists of a list of keys and a value
for insertion, we take a list of keys, and the value associated with
them
for lookup we take the list of keys and return a value, if such a pair
with such a list exists
|#
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (same-key? keys1 keys2)
      (cond ((and (null? keys1) (null? keys2)) #t)
            ((or (or (null? keys1) (null? keys2))
                 (not (equal? (car keys1) (car keys2)))) #f)
            (else (same-key? (cdr keys1) (cdr keys2)))))
    
    (define (assoc keys records)
      (cond ((null? records) false)
            ((same-key? keys (caar records)) (car records))
            (else (assoc keys (cdr records)))))

    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons keys value)
                            (cdr local-table)))))
      'ok)    

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define table (make-table))
(define get (table 'lookup-proc))
(define put (table 'insert-proc!))

(put '(5 5) 'data1)
(put '(1 'a 6 7) 'data2)
(put '('a) 'data3)
(get '(5 5))
(get '('a))
(get '(1 'a 6 7))
(get '(1 'a 6))
