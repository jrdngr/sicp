#lang racket

(require "common.rkt")
(require "numbers.rkt")

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
  (define (widthdraw amount)
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


; Exercise 3.8
; FINISH THIS
(define (f arg)
  (let ((thing 1))
    (lambda (a)
      (begin (set! thing (+ thing a))
             thing))))
  