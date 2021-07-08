#lang scheme
(define (make-account balance password)
  (define user-list (list password))

  (define incorrect-attempts 0)

  (define (add-user new-password)
    (set! user-list (cons new-password user-list)))

  (define (access? pwd)
    (define (iter pwd pwds) 
      (cond ((null? pwds) #f)
            ((eq? pwd (car pwds)) #t)
            (else (iter pwd (cdr pwds)))))
    (iter pwd user-list))

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (call-the-cops)
     (lambda (x)
       (display "too many incorrect attempts. Cops will be called!")
       (newline)))

  (define (dispatch m user-password)
    (if (access? user-password)
        (begin
          (set! incorrect-attempts 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'add-user) add-user)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (begin
          (set! incorrect-attempts (+ incorrect-attempts 1))
          (if (= incorrect-attempts 7)
              (begin
                (set! incorrect-attempts 0)
                (call-the-cops))
              (error "Incorrect Password. Access Denied" m)))))
  dispatch)

;make-joint proc
(define (make-joint account password new-password)
  ((account 'add-user password) new-password)
  account)

(define paul-acc (make-account 1000 'pwd123))
(define peter-acc (make-joint paul-acc 'pwd123 'pwd456))
((paul-acc 'deposit 'pwd123) 100)
((peter-acc 'deposit 'pwd456) 200)
((paul-acc 'deposit 'pwd123) 0)
((paul-acc 'deposit 'pwd456) 0)
