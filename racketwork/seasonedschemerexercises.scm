

;; -*- geiser-scheme-implementation: chicken -*-


(load "littleschemerexercises")

(define hi "hello")

(define eternity
  (lambda (x)
    (eternity x)))

;; returns true if first atom of given lat is a
(define (is-first? a lat)
  (cond [(null? lat) #f]
        [#t (eq? (car lat) a)]
        ))

;; define two-in-a-row? using is-first?
(define (two-in-a-row? lat)
  (cond [(null? lat) #f]
        [(is-first? (car lat) (cdr lat)) #t]
        [#t (two-in-a-row? (cdr lat))]
        ))

;; sum-of-prefixes takes in a list
;; and returns a list with prefix sum
;; a prefix sum recurrence that needs an extra argument
;; to remember what it has seen
(define (sum-of-prefixes-b seen lat)
  (cond [(null? lat) '()]
        [#t (cons (+ seen (car lat)) (sum-of-prefixes-b (+ seen (car lat)) (cdr lat)))]
        ))

;; if you notice closely, that summing operation of prefix is done
;; while going into the natural recursion
;; and consing is done while returning from the natural recurison.
;; so it is not tailrecursive
(define (sum-of-prefixes lat)
  (sum-of-prefixes-b 0 lat))

;; a function that remembers its arguments and prints the list at the end
(define (remember-arguments largs lat)
  (cond [(null? lat) largs]
        [#t (remember-arguments (cons largs (car lat)) (cdr lat))]
        ))

;; scramble is a crazy function defined in txt

;; one way to do it is make a helper function cnt-backwards
;; which uses revlist and nth to get the desired number
;; and we do a structural recursion on original list and keep
;; consing answers given by cnt-backwards

(define (cnt-backwards lat)
  (cond [(atom? lat) lat]
        [(null? lat) '()]
        [#t (pick (car lat) lat)]
        ))

;; need to send prefix list at each step to cnt-backwards
;; note we are accumulating/structurally building up data structures/answers/prefixes at the same time
;; we are drilling down structurally drilling down on last argument given tup
(define (scramble-helper largs ans tup)
  (cond [(null? tup) ans]
        [#t (scramble-helper (cons (car tup) largs) (cons (cnt-backwards (cons (car tup) largs)) ans) (cdr tup))]
        ))

;; also functions defined like above are tail call optimized because of tail transfer

(define (scramble tup)
  (reverse (scramble-helper '() '() tup)))

;; here is how the text says to do it
;; only the prefix is maintained in arguments i.e. largs
;; but the answer is returned recursively and not maintained in a argument like ans
;; try writing that on your own
;; TODO

;; motivation for using letrec
;; Regarding you specific example: Personally I find the letrec version easier to read: you define a recursive helper function and you call it in the body of the top-level function. The main difference between the two forms is that in the letrec form you don't have to specify the static arguments over and over again in the recursive calls, which I find to be cleaner.

;; Here is an example usage of letrec
(letrec ([even? (lambda (n)
                  (if (zero? n)
                      #t
                      (odd? (- n 1))))]
         [odd? (lambda (n)
                 (if (zero? n)
                     #f
                     (even? (- n 1))))])
  (even? 88))

;; general syntax of letrec is
;; (letrec ([x1 e1] ... [xn en])
;;    body)

;; can you recall what did (multirember a lat) do?
;; it was a recursive fun which removed all
;; occurences of atom a in list of atoms lat

;; the thing is we would like to define an internal
;; function mr and avoid the non changing argument a in
;; recursive definition
;; like below using letrec

(define (new-multirember a lat)
  ((letrec ([mr (lambda (lat)
                 (cond [(null? lat) '()]
                       [(eq? a (car lat)) (mr (cdr lat))]
                       [#t (cons (car lat) (mr (cdr lat)))]
                       ))])
     mr) lat))

;; prefer letrec over Y

;; a curried version of multirember is multirember-f
;; which takes necessary parameter predicate to use
;; predicate test? can be eq? or equal? etc
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a) ((multirember-f test?) a (cdr lat))]
            [#t (cons (car lat) ((multirember-f test?) a (cdr lat)))]
            ))))

;; note (multirember-f test?) does not change but we are doing it twice
;; we call multirember-f and it needs test? so wee pass it
;; if we define using letrec and recursive function m-f stays
;; as a sub function of multirember, then it is not needed to be passed

;; here is an implementation that removes non recursive atom a
;; as well as already bounded test? so it does not need to be calculated again

(define multirember-f2
  (lambda (test?)
    (lambda (a)
      (letrec ([m-f (lambda (lat)
                      (cond [(null? lat) '()]
                            [(test? (car lat) a) (m-f (cdr lat))]
                            [#t (cons (car lat) (m-f (cdr lat)))]
                            ))])
        m-f))))

;; write member? using letrec which does not include non changing atom
(define member2?
  (lambda (a lat)
    (letrec ([yes? (lambda (l)
                     (cond [(null? l) #f]
                           [(eq? (car l) a) #t]
                           [#t (yes? (cdr l))]
                           ))])
      (yes? lat))))


;; if we recall the implementation of union
;; we remove elements from one list (which changes) and lookup in another
;; list which remains same through recursion
;; so we can use letrec to redefine union
;; TODO left as exercise for the reader



;; recall that two-in-a-row was a function that determined
;; if there were more than two consecutive same atoms in a list
;; used auxillary functions like two-in-a-row-b? which can be hidden inside
;; letrec

(define two-in-a-row2?
  (lambda (lat)
    (letrec ([W (lambda (a lat)
                  (cond [(null? lat) #f]
                        [#t (or (eq? a (car lat))
                                (W (car lat) (cdr lat)))]))])
      (cond [(null? lat) #f]
            [#t (W (car lat) (cdr lat))])
      )))

;; another functions where we can use protection could be reverse
;; since it is argument is only a single list, but might need auxillary
;; recursive function that builds up reversal
;; TODO


;; we have functions
;; sum-of-prefixes-b and sum-of-prefixes.
;; we should protect sum-of-prefixes-b using letrec
(define (sum-of-prefixes2 tup)
  (letrec ([S (lambda (sss tup)
                (cond [(null? tup) '()]
                      [#t (cons (+ sss (car tup)) (S (+ sss (car tup)) (cdr tup)))]))])
    (S 0 tup)))

;; redefine scramble using letrec
;; TODO


;; intersect s1 s2 computes intersection of the two given sets
;; using member? and does structural recursion on s1 while s2 is kept same

(define (intersect2 s1 s2)
  (letrec ([I (lambda (s)
                (cond [(null? s) '()]
                      [(member? (car s) s2) (cons (car s) (I (cdr s)))]
                      [#t (I (cdr s))]
                      ))])
    (I s1)))


;; intersecctall takes a list of sets and intersects them all
;; although we have written this previously, it does not take care
;; of empty list passed to it, and we can write it with consise
;; notation using letrec

(define (intersectall2 lset)
  (letrec ([A (lambda (lset)
                (cond [(null? (cdr lset)) (car lset)]
                       [#t (intersect (car lset) (A (cdr lset)))]
                       ))])
    (cond [(null? lset) '()]
          [#t (A lset)])))


;; the names given with let, only matter inside let

;; in the above intersectall case , we would believe that
;; as soon as we encounter a empty list in lset, we would like an early return
;; instead of keeping it intersecting with the earlier lists seen.

;; enter letcc
;; where cc is call-with-current-continuation

;; first with call-with-current-continuation
(define (intersectall3 lset)
  (call-with-current-continuation ;; capture ret pt b4 starting rec
   (lambda (hop)
     (letrec ([A (lambda (lset)
                   (cond [(null? (car lset)) (hop '())] ;; early exit
                         [(null? (cdr lset)) (car lset)]
                         [#t (intersect (car lset) (A (cdr lset)))]
                         ))])
       (cond [(null? lset) '()]
             [#t (A lset)])))))


;; letcc is same as call-with-current-continuation but
;; consise syntax where (call/cc (lambda (hop) (...)))
;; is changed to        (letcc hop (...))
;; where ever result returned by ... is given back if hop is not invoked

; The same can be achieved using letcc.
; letcc is not available in r5rs Scheme. But it can be implemented
(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation
      (lambda (var)  body ... )))))


(define (intersectall4 lset)
  (letcc hop
         (letrec ([A (lambda (lset)
                       (cond [(null? (car lset)) (hop '())]
                             [(null? (cdr lset)) (car lset)]
                             [#t (intersect (car lset) (A (cdr lset)))]
                             ))])
           (cond [(null? lset) '()]
                 [#t (A lset)]
                 ))))


;; intersect all with privately defined intersect
;; that can do an early return
(define (intersectall5 lset)
  (letcc hop
         (letrec ([A (lambda (lset)
                       (cond [(null? (car lset)) (hop '())]
                             [(null? (cdr lset)) (car lset)]
                             [#t (I (car lset) (A (cdr lset)))]))]
                  [I (lambda (s1 s2)
                       (letrec ([J (lambda (s1)
                                     (cond [(null? s1) '()]
                                           [(member? (car s1) s2) (cons (car s1) (J (cdr s1)))]
                                           [#t (J (cdr s1))]))])
                         (cond [(null? s2) (hop '())]
                               [#t (J s1)])))])
           (cond [(null? lset) '()]
                 [#t (A lset)]))))


;; rember in letrec style
;; rember removes first matching element in a list
(define (rember a lat)
  (letrec ([R (lambda (lat)
                (cond [(null? lat) '()]
                      [(eq? a (car lat)) (cdr lat)]
                      [#t (cons (car lat) (R (cdr lat)))]
                      ))])
    (R lat)))

;; rember-beyond-first takes an atom and a list of atoms
;; and removes all atoms after including given atom inside the list
(define (rember-beyond-first a lat)
  (letrec ([R (lambda (lat)
                (cond [(null? lat) '()]
                      [(eq? a (car lat)) '()]
                      [#t (cons (car lat) (R (cdr lat)))]))])
    (R lat)))

;; rember-upto-last removes all atoms upto and including given item in a list
;; rember-upto-last sees an atom a
;; it should forget the pending computations,
;; and should restart the process of search through the rest of list
;; this can be achieved using continuations, i.e skipping computation
;; return all atoms after the last seen a

(define (rember-upto-last a lat)
  (letcc skip
         (letrec ([R (lambda (lat)
                       (cond [(null? lat) '()]
                             [(eq? (car lat) a) (skip (R (cdr lat)))]
                             [#t (cons (car lat) (R (cdr lat)))]))])
           (cond [(null? lat) '()]
                 [#t (R lat)]))))

;; ch14
;; proper leftmost which finds leftmost atom in list of S-expressions
(define (leftmost2 l)
  (cond [(null? l) '()]
        [(null? (car l)) (leftmost2 (cdr l))]
        [(atom? (car l)) (car l)]
        [#t (leftmost2 (car l))]
        ))

;; use let to give names to repeatedly computed expressions

;; rember1* removes leftmost occurence of given atom from a list of
;; S-expressions
(define (rember1* a l)
  (cond [(null? l) '()] ;; empty list return it back
        ;; first item in list is atom, if same remove else continue on rest
        [(atom? (car l)) (cond
                          [(eq? a (car l)) (cdr l)]
                          [#t (cons (car l) (rember1* a (cdr l)))])]
        ;; first item in list is a list
        [#t (cond
             ;; try to remove it from the first item
             ;; if it was not found in first item it would be same, 
             [(eqlist? (car l) (rember1* a (car l))) (cons (car l) (rember1* a (cdr l)))]
             [#t (cons (rember1* a (car l)) (cdr l))])]
        ))

;; rember1* with letrec
(define (rember1** a l)
  (letrec ([R (lambda (ls)
                (cond [(null? ls) '()]
                      [(atom? (car ls)) (cond
                                         [(eq? a (car ls)) (cdr ls)]
                                         [#t (cons (car ls) (R (cdr ls)))])]
                      [#t (let ([av (R (car ls))]) ;; use let to name recomputed expressions
                            (cond
                             [(eqlist? (car ls) av) (cons (car ls) (R (cdr ls)))]
                             [#t (cons av (cdr ls))]))
                          ]))])
    (R l)))

;; depth* returns depth of given s-expression
(define (depth* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [#t (cond
             [(> (depth* (cdr l)) (add1 (depth* (car l)))) (depth* (cdr l))]
             [#t (add1 (depth* (cdr l)))])]))


;; above code, depth* (cdr l) is repated 3 times
;; once in 2nd case - atom? check
;; and twice in 3rd case - #t case,
;; I would try to put let right before first cond,
;; but not so fost, since let bindings execute before let body,
;; it will result in infinite recursion.
;; put let in such a way there base case already covered
(define (depth1* l)
  (cond [(null? l) 1]
        [(atom? (car l)) (depth1* (cdr l))]
        [#t (let ([a (add1 (depth1* (car l)))]
                  [d (depth1* (cdr l))])
              (cond [(> d a) d]
                    [#t a]))]
        ))

;; reason why we have 3 conditions in * functions
;; first is base case : x
;; second is atoms which cons cell look like [1 ->]
;; third is list which [| ->]

