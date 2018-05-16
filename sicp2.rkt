#lang racket

;Helpers

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign n)
  (cond ((>= n 0) 1)
        (else -1)))

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


;Exercise 2.54
;(define (equal? x y)
;  (cond ((and (null? x) (null? y)) #t)
;        ((eq? (car x) (car y))
;         (equal? (cdr x) (cdr y)))
;        (else #f)))

(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2))
         (and (equal? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else false)))

;Exercise 2.55
(eq? (car ''abracadabra) (car (quote (quote abracadabra))))


;2.3.2 Symbolic Differentiation
(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum x y . z) ; Need to finish this
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list '+ x y))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list '* x y))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent) (** base exponent)))
        (else (list '** base exponent))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv(multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


;Exercise 2.56  Added above
;Exercise 2.57


;2.3.3 Example: Representing Sets

;(define (element-of-set? x set)
;  (cond ((null? set) false)
;        ((equal? x (car set)) true)
;        (else (element-of-set? x (cdr set)))))

;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)(union-set (cdr set1) set2)))))


;Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

