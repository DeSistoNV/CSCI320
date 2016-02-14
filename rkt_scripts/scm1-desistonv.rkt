;; Nick DeSisto
;; Scheme HW 2, Programming 1
;; I worked alone on this assignment

#lang racket
;; Returns the units (least significant of rightmost) digit of the integer n
(define (units-digit n)
  (remainder n 10))

;; Returns all but the units digit of integer n
(define (all-but-units-digit n)
  (quotient n 10))

;; (1) returns number of digits in n
(define (decimal-length n)
  (if (= n 0)
  0
  (+ 1 (decimal-length (all-but-units-digit n)
                       ))))

;; (2) returns ith digit of n
(define (ith-digit n i)
  (if (= (units-digit n) i)
  0
  (+ 1 (ith-digit (all-but-units-digit n) i))))

;; (3) returns most siginificant digit
(define (leading-digit n)
  (if (= (all-but-units-digit n) 0)
      (units-digit n)
      (leading-digit (all-but-units-digit n))))

;; (4) number of times digit d occurs in n
(define (occurrences d n)
  (if (and (= (all-but-units-digit n) 0) (= (units-digit n) 0))
      0
  (if (= (units-digit n) d)
      (+ 1 (occurrences d (all-but-units-digit n)))
      (occurrences d (all-but-units-digit n)))))

;; (5) return sum of digits in n
(define (digit-sum n)
  (if (and (= (units-digit n) 0) (= (all-but-units-digit n) 0))
      0
      (+ (units-digit n) (digit-sum (all-but-units-digit n)))))

;; (6) repeatedly apply digit-sum until result is a single number

(define (digital-root n)
  (if (= (all-but-units-digit n) 0)
  n
  (digital-root (digit-sum n))))

;; (7) backwards

(define (backwards n)
  (if (= n 0)
      0
      (+ (* (units-digit n) (expt 10 (- (decimal-length n) 1))) (backwards (all-but-units-digit n)))))








;; TESTING (1) decimal-length
(display "TESTING decimal-length")(newline)(newline)
(display "(decimal-length 6)     -->  1: ") (decimal-length 6)
(display "(decimal-length 10)    -->  2: ") (decimal-length 10)
(display "(decimal-length 1001)  -->  4: ") (decimal-length 1001)
(display "(decimal-length 348567)-->  6: ") (decimal-length 348567)
(newline)
;; TESTING (2) ith-digit
(display "TESTING ith-digit")(newline)(newline)
(display "(ith-digit 12345 1) --> 4: ")(ith-digit 12345 1)
(display "(ith-digit 12345 2) --> 3: ")(ith-digit 12345 2)
(display "(ith-digit 12345 3) --> 2: ")(ith-digit 12345 3)
(display "(ith-digit 12345 4) --> 1: ")(ith-digit 12345 4)
(display "(ith-digit 12345 5) --> 0: ")(ith-digit 12345 5)
(display "(ith-digit 1 1)     --> 0: ")(ith-digit 1 1)
(newline)
;; TESTING (3) leading-digit
(display "TESTING leading-digit")(newline)(newline)
(display "(leading-digit 5)       --> 5: ")(leading-digit 5)
(display "(leading-digit 100)     --> 1: ")(leading-digit 100)
(display "(leading-digit 2342243) --> 2: ")(leading-digit 2342243)
(display "(leading-digit 0)       --> 0: ")(leading-digit 0)
(display "(leading-digit 999)     --> 9: ")(leading-digit 9)
(newline)

;; TESTING (4) occurences
(display "TESTING occurences")(newline)(newline)
(display "(occurrences 5 12545)                             --> 2 : ")(occurrences 5 12545)
(display "(occurrences 5 93218902555555920849203845666555)  --> 10: ")(occurrences 5 93218902555555920849203845666555)
(display "(occurrences 1 56565464)                          --> 0 : ")(occurrences 1 56565464)
(display "(occurrences 1 1)                                 --> 1 : ")(occurrences 1 1)
(display "(occurrences 1 0)                                 --> 0 : ")(occurrences 1 0)
(newline)

;; TESTING (5) digit-sum
(display "TESTING digit-sum")(newline)(newline)
(display "(digit-sum 45543) --> 21: ")(digit-sum 45543)
(display "(digit-sum 1203)  --> 6 : ")(digit-sum 1203)
(display "(digit-sum 5)     --> 5 : ")(digit-sum 5)
(display "(digit-sum 0)     --> 0 : ")(digit-sum 0)
(display "(digit-sum 110)   --> 2 : ")(digit-sum 110)
(newline)

;; TESTING (6) digital-root
(display "TESTING digital-root")(newline)(newline)
(display "(digital-root 341943) --> 6: ")(digital-root 341943)
(display "(digital-root 999999) --> 9: ")(digital-root 999999)
(display "(digital-root 3)      --> 3: ")(digital-root 3)
(display "(digital-root 0)      --> 0: ")(digital-root 0)
(newline)
;; TESTING (7) backwards
(display "TESTING backwards")(newline)(newline)
(display "(backwards 12345)  --> 54321    : ")(backwards 12345)
(display "(backwards 55)     --> 55       : ")(backwards 55)
(display "(backwards 41)     --> 14       : ")(backwards 41)
(display "(backwards 1)      --> 1        : ")(backwards 1)
(display "(backwards 3948143) --> 3418493 : ")(backwards 3948143)
(newline)
      
