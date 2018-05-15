#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign n)
  (cond ((>= n 0) 1)
        (else -1)))

(define (average a b)
  (/ (+ a b) 2))

;2.1

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (cons (* s (/ n g)) (abs (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat(* (numer x) (numer y))
           (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
(define (rat-eq? x y)
  (equal? (* (numer x) (denom y))
          (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;Exercise 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))


;Section 2.2.1
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

;Exercise 2.17
;Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))


;Exercise 2.18
;Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))


;Exercise 2.20
;Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments
;that have the same even-odd parity as the first argument.  For example:
;(same-parity 1 2 3 4 5 6 7)
;(1 3 5 7)

(define (same-parity . items)
  (define (same-parity-helper stuff parity-proc result)
    (if (null? stuff)
        result
        (if (parity-proc (car stuff))
            (same-parity-helper (cdr stuff) parity-proc (append result (list (car stuff))))
            (same-parity-helper (cdr stuff) parity-proc result))))        
  (if (even? (car items))
      (same-parity-helper items even? null)
      (same-parity-helper items odd? null)))


(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

;Exercise 2.23
(define (for-each proc items)
  (cond
    ((null? items)
        null)
    (else
     (proc (car items))
     (for-each proc (cdr items)))))


;2.2.2

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Exercise 2.27
;Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument
;and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,