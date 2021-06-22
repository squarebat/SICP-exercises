#lang sicp
;generic procs
(define (for-each proc items)
  (if (not (null? items))
      ((lambda ()
        (proc (car items))
        (for-each proc (cdr items))))))
;huffman tree
;leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;initial symbol-weight set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;huffman tree logic
(define (successive-merge leaf-set)
  (define (merge current next)
    (make-code-tree current next))
  (cond ((null? leaf-set) '())
        ((= (length leaf-set) 1) (car leaf-set))
        ((successive-merge (adjoin-set (merge (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (encode-symbol symbol tree)
  (define (symbol-exists? symbols-list)
    (cond ((null? symbols-list) false)
        ((equal? symbol (car symbols-list)) true)
        (else (symbol-exists? (cdr symbols-list)))))
  (cond ((leaf? tree) '())
        ((symbol-exists? (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((symbol-exists? (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol -- SYMBOL DOESN'T EXIST" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (display-code code)
  (newline)
  (display "Length of code: ")
  (display (length code))
  (newline)
  (for-each (lambda (bit) (display bit)) code)
  (newline))
;test
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display-code (encode (decode sample-message sample-tree) sample-tree))
(decode sample-message sample-tree)

(define song-words-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA
               GET A JOB SHA NA NA NA NA NA NA NA NA
               WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP 
               SHA BOOM))

(define song-code (encode song song-words-tree))
(display-code song-code)
(decode song-code song-words-tree)

