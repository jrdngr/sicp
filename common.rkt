#lang racket

; Useful functions

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign n)
  (cond ((>= n 0) 1)
        (else -1)))

(define (square n)
  (* n n))

(define (average a b)
  (/ (+ a b) 2))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (** base exponent)
  (define (exponent-helper base exponent result)
    (if (= exponent 1)
        (* base result)
        (exponent-helper base (- exponent 1) (* result base))))
  (exponent-helper base exponent 1))

(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2))
         (and (equal? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else false)))

; List stuff

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc items)
  (cond
    ((null? items)
        null)
    (else
     (proc (car items))
     (for-each proc (cdr items)))))



; Type tags

(define (attach-tag type-tag contents)
  (cons (type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datun -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))



(provide (all-defined-out))