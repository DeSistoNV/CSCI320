#lang racket
; I worked alone on this assignment.

;; (1) Sums a list using non-tail recursion
(define (sum l)
    ;(display l)(newline)
  (cond
    ((null? l) ; if list is null
     0)
    ((number? (car l)) ; if first element is a number
     (+ (car l) (sum (cdr l))))
    (else
     (+ (sum (car l)) (sum (cdr l)))
     )))


;; TESTING (1) sum
(display "TESTING sum")(newline)(newline)
(display "(sum '(2 5 (3 5) 1))                       -->  16 : ") (sum '(2 5 (3 5) 1))
(display "(sum '(1 1 1 1 1 1 1 10))                  -->  17 : ") (sum '(1 1 1 1 1 1 1 10))
(display "(sum '( (2 5 6) 1 117))                    -->  131: ") (sum '( (2 5 6) 1 117))
(display "(sum '(1 2 ((((3)))) 4 5 (1 2 (((3))) 4))) -->  25 : ") (sum '(1 2 ((((3)))) 4 5 (1 2 (((3))) 4)))
(newline)


;; interface for sum2
(define (sum2 l)
  (sum2-hidden l 0))

;; (1) Sums a list using non-tail recursion
(define (sum2-hidden l x)
   ; (display l)(display "   ")(display x)(newline)
  (cond
    ((null? l) ; if list is null
     x)
    ((number? (car l)) ; if first element is a number
     (+ (car l) (sum2-hidden (cdr l) x)))
    (else
     (sum2-hidden (cdr l) (+ x (if (number? (car l)) l (sum2-hidden (car l) 0))))
     )))


;; TESTING (2) sum2
(display "TESTING sum2")(newline)(newline)
(display "(sum2 '(2 5 (3 5) 1))                                    -->  16 : ")
(sum2 '(2 5 (3 5) 1))
(display "(sum2 '(1 1 1 1 1 1 1 10))                               -->  17 : ")
(sum2 '(1 1 1 1 1 1 1 10))
(display "(sum2 '( (2 5 6) 1 117))                                 -->  131: ")
(sum2 '( (2 5 6) 1 117))
(display "(sum2 '(1 2 ((((3)))) 4 5 (1 2 (((3))) 4)))              -->  25 : ")
(sum2 '(1 2 ((((3)))) 4 5 (1 2 (((3))) 4)))
(display "(sum2 '(1 2 ((((3 ((5)) 2 3)))) 4 5 (1 2 (((3)) 3)) 4))) -->  38 : ")
(sum2 '((1 2) ((((3 ((5)) 2 3)))) 4 5 (1 2 (((3)) 3)) 4))
(newline)

;; interface for sumNumbers
(define (sumNumbers l)
  (sumNumbers-hidden l 0))

;; (3) Sums all the numbers in a list that may contain other things
(define (sumNumbers-hidden l x)
   ;(display l)(display "   ")(display x)(newline)
  (cond
    ((null? l) ; if list is null
     x)
    ((number? (car l)) ; if first element is a number
     (+ (car l) (sumNumbers-hidden (cdr l) x)))
    ((not (list? (car l))) ; if first element is not a list and not a number
     (+ 0 (sumNumbers-hidden (cdr l) x)))
    (else
     (sumNumbers-hidden (cdr l) (+ x (if (number? (car l)) l (sumNumbers-hidden (car l) 0))))
     )))


;; TESTING (3) sumNumbers
(display "TESTING sumNumbers")(newline)(newline)
(display "(sumNumbers'(2 5 (3 5) 1))                                    -->  16 : ")
(sumNumbers '(2 5 (3 5) 1))
(display "(sumNumbers '(1 x 1 c 1 y 1 10))                              -->  14 : ")
(sumNumbers '(1 x 1 c 1 y 1 10))
(display "(sumNumbers '( (2 pizza 6) x 117))                            -->  125: ")
(sumNumbers '( (2 pizza 6) x 117))
(display "(sumNumbers '(1 2 ((((y)))) 4 5 (1 2 (((3))) a)))             -->  18 : ")
(sumNumbers '(1 2 ((((y)))) 4 5 (1 2 (((3))) a)))
(display "(sumNumbers '(x x ((((x ((x)) 2 x)))) x x (1 x (((x)) 3)) y)) -->  6  : ")
(sumNumbers '(x x ((((x ((x)) 2 x)))) x x (1 x (((x)) 3)) y))
(display "(sumNumbers '(2 q (3 ((5)))(((x)))))                          -->  10 : ")
(sumNumbers '(2 q (3 ((5)))(((x))))) 
(newline)


; interface for remove_odd
(define (remove_odd l)
  (remove_odd-hidden '() l)
  )

; (4) remove odd numbers from a list
; precondition : list contains only atoms
(define (remove_odd-hidden n l)
  ;(display l)(display "   ")(display n)(newline)
  (cond
    ((null? l) n)
    ((or (not (number? (car l))) (= (modulo (car l) 2) 0)) (remove_odd-hidden (append n (list(car l))) (cdr l) ))
    (else (remove_odd-hidden  n (cdr l)))
    ))

;; TESTING (4) remove_odd
(display "TESTING remove_odd")(newline)(newline)
(display "(remove_odd ‘(2 5 3 x 6 1))    -->  (2 x 6 2 2 2 2) : ") (remove_odd '(2 5 3 x 6 1))
(display "(remove_odd '( 2 3 4 5 6 7))   -->  (2 4 6)         : ") (remove_odd '(2 3 4 5 6 7))
(display "(remove_odd '( 1 1 1 1 1 100)) -->  (100)           : ") (remove_odd '(1 1 1 1 1 100))

(newline)


;; remove all odds from a list that may contain lists
(define (remove_odd2 l)
  (remove_odd2-hidden '() l)
  )

(define (remove_odd2-hidden n l)
  (display l)(display "   ")(display n)(newline)
  (cond
    ((null? l) n)
    ((list? (car l)) (remove_odd2-hidden n (car l)))
    (else (filter even? l))
    ))

;(display "TESTING remove_odd2") (newline) (newline)
;(display "(remove_odd2 '(2 5 (x (5)) 1)) => (2 (x, ())) : ") (remove_odd2 '(2 5 (x (5)) 1))
;(display "(remove_odd '( 2 3 4 5 6 7))  -->  (2 4 6) : ") (remove_odd2 '(2 3 4 5 6 7))
(newline)



;; (6) returns true if lists are equivalently structured false if not
(define (struct-eq l1 l2)
  ;(display l1)(display "   ")(display l2)(newline)
  (cond
    ( (and (null? l1) (null? l2)) #t )
    ( (or  (null? l1) (null? l2)) #f )
    ( (and (list? (car l1)) (list? (car l2))) (struct-eq (cdr l1) (cdr l2)))
    ( (and (not(list? (car l1))) (not(list? (car l2)))) (struct-eq (cdr l1) (cdr l2)))
    (else #f)
    ))

;; TESTING (6) struct-eq
(display "TESTING remove_odd")(newline)(newline)
(display "(struct-eq '(1 2 3) '(e r h))    --> #t : ") (struct-eq '(1 2 3) '(e r h))
(display "(struct-eq '(1 2 3) '(1 3 5 7))  --> #f : ") (struct-eq '(1 2 3) '(1 3 5 7))
(display "(struct-eq '(1 2 3) '(e (r) h))  --> #f : ") (struct-eq '(1 2 3) '(e (r) h))
(display "(struct-eq '(1 (2 3)) '(e (r h)))--> #t : ") (struct-eq '(1 (2 3)) '(e (r h)))

(newline)


;; (7) returns only items in list where f returns true
;; precondition : l is a list of items that are appropriate for the predicate f
(define (filter f l)
  (cond
    ( (null? l) '())
    ( (f (car l)) (cons (car l) (filter f (cdr l) ) ))
    (else (filter f (cdr l)))))


;; TESTING (7) filter
(display "TESTING filter")(newline)(newline)
(display "(filter zero? '(1 1 0 2 0 9))--> (0 0) : ") (filter zero? '(1 1 0 2 0 9))
(display "(filter list? '(1 () (1 2 3) 2 0 9))--> (() (1 2 3)) : ") (filter list? '(1 () (1 2 3) 2 0 9))
(display "(filter odd? '(1 1 0 2 0 9))--> (1 1 9) : ") (filter odd? '(1 1 0 2 0 9))
(display "(filter even? '(1 1 0 2 0 9))--> (0 2 0) : ") (filter even? '(1 1 0 2 0 9))
(newline)



;; TESTING (8) construction

(define (construction l n)
  (display l)(display "   ")(display n)(newline)
  (cond
    ((null? l) '())
    (cons ((car l) n) (construction (cdr l) n))
  ))

(define (g  x)
  (* x x))
(define (h x)
  (2 * x))
(define (i x)
  (/ x 2))


;(construction '(g h i) 4)

;((car '(h)) 4)

         