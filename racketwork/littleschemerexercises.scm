;; -*- geiser-scheme-implementation: guile -*-


;; an atom is something that is not a null list
;; and not a pair/list
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


;; replace first occurence of old
;; in lat with new
(define (subst new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons new (cdr lat))]
        [#t (cons (car lat) (subst new old (cdr lat)))])
  )

;; subst2 replaces first occurence of o1 or o2 by new
(define (subst2 new o1 o2 lat)
  (cond [(null? lat) '()]
        [(or (eq? o1 (car lat)) (eq? o2 (car lat)))
         (cons new (cdr lat))]
        [#t (cons (car lat) (subst2 new o1 o2 (cdr lat)))]
        ))

;; new is vanilla
;; o1 is 'chocolate
;; o2 is 'banana
;; lat is '(banana ice cream with chocolate topping)
;; subst2 should give (vanilla ice cream with chocolate topping)

;; multirember will remove all occurences of
;; an atom from a list
(define (multirember a lat)
  (cond [(null? lat) '()]
        [(eq? a (car lat)) (multirember a (cdr lat))]
        [#t (cons (car lat) (multirember a (cdr lat)))]
        ))

;; a is cup
;; lat is '(coffee cup tea cup and hick cup)
;; ans is '(coffee tea and hick)

;; now do multisubst mimicking multirember
;; TODO


;; chapter: Numbers

(define (add1 n)
  (+ n 1))

(define (sub1 n)
  (- n 1))

;; zero? checks if argument is 0

(define (plus m n)
  (cond [(zero? m) n]
        [#t (add1 (plus (sub1 m) n))]
        ))

(define (subs m n)
  (cond [(zero? n) m]
        [#t (sub1 (subs m (sub1 n)))]
        ))

(define (addtup tup)
  (cond [(null? tup) 0]
        [(plus (car tup) (addtup (cdr tup)))]
        ))


(define (mult n m)
  (cond [(zero? m) 0]
        [#t (plus n (mult n (sub1 m)))]
        ))


;; add two lists of numbers
;; (tupPlus '(3 7) '(4 6 7 1))
(define (tupPlus tup1 tup2)
  (cond [(and (null? tup1) (null? tup2)) '()]
        [(null? tup1) tup2]
        [(null? tup2) tup1]
        [#t (cons (plus (car tup1) (car tup2)) (tupPlus (cdr tup1) (cdr tup2)))]
        ))


;; implement (greater fst snd)
;; returns true if fst is greater than snd
;; using numerical recursion (sub1)

(define (greater fst snd)
  (cond [(zero? fst) #f]
        [(zero? snd) #t]
        [#t (greater (sub1 fst) (sub1 snd))]
        ))

;; return true if fst is lessThan snd
(define (lessThan fst snd)
  (cond [(zero? snd) #f]
        [(zero? fst) #t]
        [#t (lessThan (sub1 fst) (sub1 snd))]
        ))

;; exp is a builtin function, so we use expo
;; expo n m = n^m
(define (expo n m)
  (cond [(zero? m) 1]
        [#t (mult n (expo n (sub1 m)))]
        ))

;; (div fst snd) returns
;; quotient when fst is divided by snd
(define (div fst snd)
  (cond [(lessThan fst snd) 0]
        [#t (add1 (div (subs fst snd) snd))]
        ))

;; here is the structural recurion behind (div 15 4)
;; 1 + (div 11 4)
;; 1 + 1 + (div 7 4)
;; 1 + 1 + 1 + (div 3 4)
;; 1 + 1 + 1 + 0
;; = 3


;; length is a builtin function so we will name it
;; (len lst) gives us length of lst
(define (len lst)
  (cond [(null? lst) 0]
        [#t (add1 (len (cdr lst)))]
        ))


;; (pick n lst) picks nth atom from the lst if exists
(define (pick n lst)
  (cond [(null? lst) '()]
        [(eq? n 1) (car lst)]
        [#t (pick (sub1 n) (cdr lst))]
        ))

;; (rempick n lst) removes after picking nth item in list
(define (rempick n lst)
  (cond [(null? lst) '()]
        [(eq? n 1) (cdr lst)]
        [#t (cons (car lst) (rempick (sub1 n) (cdr lst)))]
        ))

;; (no-nums lst) removes all numbers from lst using
;; predicate number?
(define (no-nums lst)
  (cond [(null? lst) '()]
        [(number? (car lst)) (no-nums (cdr lst))]
        [#t (cons (car lst) (no-nums (cdr lst)))]
        ))
;; (no-nums '(4 pears 4 prunes 8 dates))

;; (all-nums lst) gives list of all numbers in list
(define (all-nums lst)
  (cond [(null? lst) '()]
        [(number? (car lst)) (cons (car lst) (all-nums (cdr lst)))]
        [#t (all-nums (cdr lst))]
        ))

;; (occur a lst) tells number of times
;; an atom a occurs in lst

(define (occur a lst)
  (cond [(null? lst) 0]
        [(eq? a (car lst)) (add1 (occur a (cdr lst)))]
        [#t (occur a (cdr lst))]
        ))


;; ch 5
;; (rember* a l)
;; removes a from nested lists l
;; like recursing on trees based on types of atom
;; we are looking at
(define (rember* a l)
  (cond [(null? l) '()]
        [(list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))]
        [(eq? (car l) a) (rember* a (cdr l))]
        [#t (cons (car l) (rember* a (cdr l)))]
        ))

;; (insertR* new old l)
;; insertR* inserts atom new to the right of old
;; irrespective of where old occurs

(define (insertR* new old l)
  (cond [(null? l) '()]
        [(list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l)))]
        [(eq? (car l) old) (cons (car l) (cons new (insertR* new old (cdr l))))]
        [#t (cons (car l) (insertR* new old (cdr l)))]
        ))

;; occur* checks number of occurences of atom
;; in a nested list

(define (occur* a l)
  (cond [(null? l) 0]
        [(list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l)))]
        [(eq? (car l) a) (+ 1 (occur* a (cdr l)))]
        [#t (occur* a (cdr l))]
        ))


;; subst* replaces all occurences of old with new
;; in a nested list

(define (subst* new old l)
  (cond [(null? l) '()]
        [(list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l)))]
        [(eq? (car l) old) (cons new (subst* new old (cdr l)))]
        [#t (cons (car l) (subst* new old (cdr l)))]
        ))

;; insertL* inserts a new item to the left of old
;; in a nested list

(define (insertL* new old l)
  (cond [(null? l) '()]
        [(list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l)))]
        [(eq? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
        [#t (cons (car l) (insertL* new old (cdr l)))]
        ))

;; member* finds if a is a member of l
;; where l is a nested list
(define (member* a l)
  (cond [(null? l) #f]
        [(list? (car l)) (or (member* a (car l)) (member* a (cdr l)))]
        [(eq? (car l) a) #t]
        [#t (member* a (cdr l))]
        ))

;; (leftmost l) returns leftmost atom
;; in a nested list l

(define (leftmost l)
  (cond [(null? l) '()]
        [(list? (car l)) (leftmost (car l))]
        [#t (car l)]
        ))

;; eqlist? is a function that takes two arguments
;; and tells if they are equal
;; writing eqlist? using equal?

(define (eqlist? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(or (null? l1) (null? l2)) #f]
        [#t (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)) )]
        ))

;; now we would use equal? in places we used eq?
;; and all our our functions which took an atom and list
;; could be generalized to take an S-expression and
;; a list of s-expressions
;; rember removes first matching s in ls
(define (s-rember s ls)
  (cond [(null? ls) '()]
        [(equal? s (car ls)) (cdr ls)]
        [#t (cons (car ls) (s-rember s (cdr ls)))]
        ))

;; given a quoted mathematical expression with infix operators
;; e.g. '(1 + (3 * 4))
;; returns true if all non-operators in the expression above are numbers

(define (numbered? aexp)
  (cond [(null? aexp) #t]
        [(atom? aexp) (number? aexp)]
        [(eq? '+ (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
        [(eq? '* (car (cdr aexp))) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))]
        [#t #f]
        ))


;; let's abstract out the car's and cdr's for arithmetic expressions
;; first-sub-exp
(define (first-sub-exp aexp)
  (car aexp))

(define (second-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car (cdr aexp)))

;; now we calculate value of quoted infix arithmetic expressions
;; using our domain abstraction functions that we built above

(define (value aexp)
  (cond [(null? aexp) 0]
        [(number? aexp) aexp]
        [(eq? '+ (operator aexp)) (+ (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))]
        [(eq? '* (operator aexp)) (* (value (first-sub-exp aexp)) (value (second-sub-exp aexp)))]
        [#t 0]
        ))

;; ch7

;; member? checks if an item is in a list
(define (member? s ls)
  (cond [(null? ls) #f]
        [(equal? s (car ls)) #t]
        [#t (member? s (cdr ls))]
        ))

;; set? tells if a list of atoms is a set or not

(define (set? lat)
  (cond [(null? lat) #t]
        [(member? (car lat) (cdr lat)) #f]
        [#t (set? (cdr lat))]
        ))

