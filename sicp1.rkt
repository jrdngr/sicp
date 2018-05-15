#lang racket

(define (square n)
  (* n n ))
(define (cube n)
  (* n n n))

;1.2.2
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  

;Exercise 1.11
;A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) +
;3f(n - 3) if n>= 3. Write a procedure that computes f by means of a recursive process. Write a procedure
;that computes f by means of an iterative process.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (g n)
  (g-iter 2 1 0 n))
(define (g-iter a b c count)
  (if (= count 0)
      c
      (g-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))


;Exercise 1.12
;Write a procedure that computes elements of Pascal’s triangle by means of a
;recursive process.


;1.2.4
(define (expt-sad b n)
  (if (= n 0)
      1
      (* b (expt-sad b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
  ((even? exp)
   (remainder (square (expmod base (/ exp 2) m))
              m))
  (else
   (remainder (* base (expmod base (- exp 1) m))
              m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;1.3
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))


;Exercise 1.31
(define (product term a next b)
  (if (> a b)
      0
      (* (term a)
         (product term (next a) next b))))

;Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                 (accumulate combiner null-value term (next a) next b))))



;1.3.2

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))


(define (f2 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f3 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f4 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))


;Exercise 1.34
(define (g1 g2)
  (g2 2))



;1.3.3


;1.3.4

(define (average a b)
  (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))