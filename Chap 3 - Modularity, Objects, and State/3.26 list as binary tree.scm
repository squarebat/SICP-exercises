#lang sicp
;Tree representation from ex 2.65
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (balance-tree tree)
  (list->tree (tree->list tree)))

;[EX 3.26 STARTS HERE] arbitrary keys table representation as balanced binary tree
(define (make-table)
  (let ((local-table (list '*table*)))

    (define (greater-than? keys1 keys2)
      (cond ((and (null? keys1) (null? keys2)) false)
            ((or (null? keys1)
                 (and (not (null? keys2)) (< (car keys1) (car keys2)))) false)
            ((or (null? keys2)
                 (> (car keys1) (car keys2))) true)
            (else (greater-than? (cdr keys1) (cdr keys2)))))
    
    (define (less-than? keys1 keys2)
      (not (greater-than? keys1 keys2)))
    
    (define (same-key? keys1 keys2)
      (cond ((and (null? keys1) (null? keys2)) #t)
            ((or (or (null? keys1) (null? keys2))
                 (not (equal? (car keys1) (car keys2)))) #f)
            (else (same-key? (cdr keys1) (cdr keys2)))))
    
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (car (entry records))) (entry records))
            ((less-than? key (car (entry records)))
             (assoc key (left-branch records)))
            ((greater-than? key (car (entry records)))
             (assoc key (right-branch records)))))
    
    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (adjoin pair records)
      (balance-tree
       (cond ((null? records) (make-tree pair '() '()))
             ((less-than? (car pair) (car (entry records)))
              (make-tree (entry records) 
                         (adjoin pair (left-branch records))
                         (right-branch records)))
             ((greater-than? (car pair) (car (entry records)))
              (make-tree (entry records)
                         (left-branch records)
                         (adjoin pair (right-branch records)))))))


    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (adjoin (cons keys value)
                            (cdr local-table)))))
      'ok)    

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;tests on figure 2.16
(define table (make-table))
(define get (table 'lookup-proc))
(define put (table 'insert-proc!))

(put '(5 5) 'data1)
(put '(1 2 6 7) 'data2)
(put '(1) 1)
(put '(1 1) 2)
(put '(1 1 1) 3)
(get '(5 5))
(get '(1 2 6 7))
(get '(1 2 6))
(get '(1))
(get '(1 1))
(get '(1 1 1))