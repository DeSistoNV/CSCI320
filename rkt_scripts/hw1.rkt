#lang racket
;;; Practice
;;;
;;; 1. Multiply two integers 
(define (multiply x y)
  (if (or (= x 0) (= y 0)) 
      0
      (+
       (if (and (negative? x) (negative? y)) (- x) x)
         (multiply
          (if (and (negative? x) (negative? y)) (- x) x)
          (sub1 (if (and (negative? x) (negative? y)) (- y) y)
                    )))))

;;; 2.  A Pythagorean triple is a tuple of integers (x, y, z) such that x * x + y * y = z * z.
;;; Write a function with a parameter n to print all Pythagorean triples
;;; such that 1 ≤ x ≤ y ≤ z ≤ n.

(define (pythag_triplets n)
  (if (< n 3)
      0 0
      ))

;;; 3. print Nth digit of fibonacci sequence
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 2)) (fib (- n 1))
      )))

(= (multiply 1 2 ) 2)
(= (multiply 5 5) 25)
(= (multiply 7 9) 63)
(= (multiply 100 0) 0)
(= (multiply 100 17) 1700)
(= (multiply -100 -17) 1700)
(= (multiply -1 30) -30)


(= (fib 1) 1)
(= (fib 2) 1)
(= (fib 3) 2)
(= (fib 4) 3)
(= (fib 5) 5)
(= (fib 6) 8)
(= (fib 7) 13)
(= (fib 8) 21)
(= (fib 9) 34)
(= (fib 10) 55)