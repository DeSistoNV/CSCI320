#lang racket
; Nick DeSisto
; CSCI 320 : ML & Scheme data structures assignment
; I worked on this submission alone.


; code provided by textbook 
(define (leftchild B) (car ( cdr B)))
(define (rightchild B) (car (cdr (cdr B))))
(define (data B) (car B))


; print tree function provided by textbook
(define print-tree (lambda (B)
(cond ((null? B) '() )
(else (print-tree (leftchild B))
(display (data B))
(newline)
(print-tree (rightchild B))))))



; returns true if val is in tree else false
(define (find val tree)

  (cond

    ((null? tree) #f)
    
    ((string=? val (data tree)) #t)

    ((string<? val (data tree)) (find val (leftchild tree)))
    ((string>? val (data tree)) (find val (rightchild tree)))
    ))

; returns the count of nodes with only one non-null child
(define (parentsOfOne tree)
  (cond
    ( (null? tree) 0) ; if root is null
    ( (null? (data tree)) 0)  ; not needed, but cant hurt
    ( (and (null? (leftchild tree)) (null? (rightchild tree))) 0) ; if left and right are null
    ( (and (not(null? (leftchild tree))) (not(null? (rightchild tree)))) 
      (+ (parentsOfOne (leftchild tree)) (parentsOfOne (rightchild tree)))) ; if neither are null

    ( (not(null?(leftchild tree))) (+ 1 (parentsOfOne (leftchild tree)))) ; if left is null
    ( (not(null?(rightchild tree))) (+ 1(parentsOfOne (rightchild tree)))) ; if right is null
    )) 




; TESTING
(print-tree
  '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" () ("pizza" () ())) () ))
  )

(parentsOfOne 
 '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" () ("pizza" () ())) () ))
)



;; TESTING (1) find
(display "TESTING find")(newline)(newline)
(display "Testing on  t :=  '(\"horse\" (\"cow\" () (\"dog\" () ()))
           (\"zebra\" (\"yak\" (\"pizza\" () ()) ()) () )))")(newline)
(display "(find \"yak\" t)    -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))

(display "(find \"cow\" t)    -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))

(display "(find \"horse\" t)  -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))

(display "(find \"pizza\" t)  -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))

(display "(find \"zebra\" t)  -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))
(display "(find \"dog\" t)    -->  #t : ")
(find "zebra" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))
(display "(find \"llama\" t)  -->  #f : ")
(find "llama" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))
(display "(find \"piz\" t)    -->  #f : ")
(find "piz" '("horse" ("cow" () ("dog" () ()))
           ("zebra" ("yak" ("pizza" () ()) ()) () )))
(newline)(newline)



;; TESTING (1) find
(display "TESTING find")(newline)(newline)
(display "Testing parentsOfOne")(newline)
(display "(parentsOfOne '(\"head\" () ()) )    -->  0 : ")
(parentsOfOne '("head" () ()))

(display "(parentsOfOne '(\"head\" (\"left\" () ()) ()) )    -->  1 : ")
(parentsOfOne '("head" ("left" () ()) ()))


(display "(parentsOfOne '(\"head\" () (\"right\" () ())) )    -->  1 : ")
(parentsOfOne '("head" () ("right" () ()) ))

(display "(parentsOfOne '(\"head\" (\"left\" (\"apple\" (\"abc\" () ()) ()) ())
              (\"right\" () (\"yellow\" (\"soup\" () ()) ()))))    -->  4 : ")
(parentsOfOne '("head" ("left" ("apple" ("abc" () ()) ()) ())
              ("right" () ("yellow" ("soup" () ()) ()))))