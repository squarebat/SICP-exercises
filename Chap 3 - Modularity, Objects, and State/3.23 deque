#lang sicp
#|
Representation: for deque, we maintain a doubly linked list. In normal queue we just link an item with the item next to it.
In deque, we link and item to both previous item and the next item.

For single item we create a pair, where first element is the vale, the next is a pair pointing to previous and next item.

(item-val (prev next))
|#
(define (make-node item) (cons '() item))
(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-deque?) (null? front-ptr))

    (define (front-deque)
      (if (empty-deque?)
          (error "FRONT called with an empty deque" front-ptr)
          (caar front-ptr)))

    (define (rear-deque)
      (if (empty-deque?)
          (error "REAR called with an empty deque" front-ptr)
          rear-ptr))
    
    (define (front-insert-deque! item)
      (let ((new-node (make-node item)))
        (let ((new-pair (cons new-node '())))
        (cond ((empty-deque?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (get-items front-ptr))
              (else
               ;make new-pair the front ptr and doubly link new item with previous first item
               (set-cdr! new-pair front-ptr)
               (set-car! (car front-ptr) new-pair)
               (set-front-ptr! new-pair)
               (get-items front-ptr))))))
    
    (define (rear-insert-deque! item)
      (let ((new-node (make-node item)))
        (let ((new-pair (cons new-node '())))
          (cond ((empty-deque?)
                 (set-front-ptr! new-pair)
                 (set-rear-ptr! new-pair)
                 (get-items front-ptr))
                (else
                 ;doubly link new item with previous rear item
                 (set-cdr! rear-ptr new-pair)
                 (set-car! (car new-pair) rear-ptr)
                 (set-rear-ptr! new-pair)
                 (get-items front-ptr))))))

    (define (front-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE! called with an empty deque" front-ptr))
            (else
             (set-front-ptr! (cdr front-ptr))
             (set-car! (car front-ptr) '())
             (get-items front-ptr))))

    (define (rear-delete-deque!)
      (cond ((empty-deque?)
             (error "DELETE! called with an empty deque" front-ptr))
            (else
             (set-rear-ptr! (caar rear-ptr))
             (set-cdr! rear-ptr '())
             (get-items front-ptr))))

    (define (get-items front-ptr)
      (if (null? front-ptr)
          '()
          (cons (cdar front-ptr) (get-items (cdr front-ptr)))))
    
    (define (dispatch m)
      (cond ((eq? m 'front-insert) front-insert-deque!)
            ((eq? m 'front-delete) front-delete-deque!)
            ((eq? m 'rear-insert) rear-insert-deque!)
            ((eq? m 'rear-delete) rear-delete-deque!)
            ((eq? m 'rear) rear-ptr)
            (else (error "ERROR! Invalid operation called on deque!" front-ptr))))
    dispatch))

;test
(define q (make-deque))
((q 'front-insert) 1)
((q 'front-insert) 2)
((q 'front-insert) 3)
((q 'rear-insert) 0)
((q 'rear-insert) -1)
((q 'front-delete))
((q 'rear-delete))



