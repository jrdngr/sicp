#lang sicp

(#%require "common.rkt")
(#%require "numbers.rkt")

; 3.1.1
; Local State Variables

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)
  
;(define acc (make-account 100))
;((acc 'withdraw) 50)
;50
;((acc 'withdraw) 60)
;"Insufficient funds"
;((acc 'deposit) 40)
;90
;((acc 'withdraw) 60)
;30


; Exercise 3.1
(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

; Exercise 3.2
(define (make-monitored proc)
  (let ((num-calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) num-calls)
          (else (begin (set! num-calls (+ num-calls 1))
                       (proc arg)))))))

; Exercise 3.3
(define (make-account-pass balance password)
  (define (widthdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (eq? pass password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        (lambda (dummy) "Incorrect password")))
  dispatch)

; 3.1.3 -- The Costs of Introducing Assignment

; The trouble here is that substitution is based ultimately on the notion that the symbols in our language are essentially names for values. 
; But as soon as we introduce set! and the idea that the value of a variable can change, a variable can no longer be simply a name. 
; Now a variable somehow refers to a place where a value can be stored, and the value stored at this place can change. 
; In 3.2 we will see how environments play this role of “place” in our computational model. 



; Queue

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



