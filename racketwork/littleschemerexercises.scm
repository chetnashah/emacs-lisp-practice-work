;; -*- geiser-scheme-implementation: chicken -*-


;; an atom is something that is not a null list
;; and not a pair/list
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

;; note pair? returns true on all elements list with one or more item
;; esentially as they are cons made
;; e.g. (pair? '(1)) is #t


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
;; since set? is based on member? which is based on equal?
;; it works with lists as items
;; e.g. (set? '((1 2) (2 1) (1 2)))

;; makeset takes a list and returns a unique list(which is set)
(define (mymakeset lat)
  (cond [(null? lat) '()]
        [(member? (car lat) (cdr lat)) (mymakeset (cdr lat))]
        [#t (cons (car lat) (mymakeset (cdr lat)))]
        ))

;; makeset using multirember, when encounter an element remove it
;; from rest of list
(define (mymakeset-rember lat)
  (cond [(null? lat) '()]
        [#t (cons (car lat) (multirember (car lat) (mymakeset-rember (cdr lat))))]
        ))

;; another way to write mymakeset based on multirember
(define (mymakeset-rember2 lat)
  (cond [(null? lat) '()]
        [#t (cons (car lat) (mymakeset-rember2 (multirember (car lat) (cdr lat))))]
        ))

;; standard set operations
;; subset, eqset
;; set algebra operations : intersection, union

;; return true if set1 is a subset of set2
(define (subset? set1 set2)
  (cond [(null? set1) #t]
        [#t (and (member? (car set1) set2) (subset? (cdr set1) set2))]
        ))

(define (eqset? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

;; intersect? predicate if there is atleast one element common between given sets
(define (intersect? set1 set2)
  (cond [(null? set1) #f]
        [#t (or (member? (car set1) set2) (intersect? (cdr set1) set2))]
        ))

;; compare and contrast methods subset? and intersect?

;; intersect takes two sets and returns the intersection of two sets
(define (intersect set1 set2)
  (cond [(null? set1) '()]
        [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
        [#t (intersect (cdr set1) set2)]
        ))

;; union takes two sets and returns the union of two sets
;; union uses multirember
(define (union set1 set2)
  (cond [(null? set1) set2]
        [#t (cons (car set1) (union (cdr set1) (multirember (car set1) set2)))]
        ))
;; another way to write union without multi rember is to ignore the elment of set1
;; present in set2 checked via member?


;; set-diff takes two sets and returns set1 - set2
(define (set-diff set1 set2)
  (cond [(null? set1) '()]
        [(member? (car set1) set2) (set-diff (cdr set1) set2)]
        [#t (cons (car set1) (set-diff (cdr set1) set2))]
        ))

;; intersectall takes a list of sets : l-set and returns the intersection
;; of all the sets

(define (intersect-all l-set)
  (cond [(null? l-set) '()]
        [(null? (cdr l-set)) (car l-set)];; singleton intersection is self
        [#t (intersect (car l-set) (intersect-all (cdr l-set)))]
        )) ;; we folded in the last step using combiner intersect


;; firsts takes a list of pairs and returns list of all first elements
;; of pairs
(define (firsts l)
  (cond [(null? l) '()]
        [#t (cons (car (car l)) (firsts (cdr l)))]
        ))
;; (firsts '((a b) (c d) (e f)))
;; is '(a c e)


(define (fst pr)
  (car pr))

(define (snd pr)
  (car (cdr pr)))

(define (make-pair a b)
  (cons a (cons b '())))

;; fun? takes a relation (list of pairs) and tells if it can
;; be a function (unique cars)

(define (fun? rel)
  (set? (firsts rel)))

;; revrel takes a rel and reverses all the pairs in relation
(define (revrel rel)
  (cond [(null? rel) '()]
        [(pair? (car rel)) (cons (make-pair (snd (car rel)) (fst (car rel))) (revrel (cdr rel)))]
        [#t (error "did not find a pair")]
        ))

;; also could have used a utility function that reverses a given pair
(define (revpair pr)
  (make-pair (snd pr) (fst pr)))

;; seconds is a function that takes a list of pairs/rel and returns a list containing
;; all the second items in pair
(define (seconds rel)
  (cond [(null? rel) '()]
        [(pair? (car rel)) (cons (snd (car rel)) (seconds (cdr rel)))]
        [#t (error "did not find a pair")]
        ))

;; write a funciton one-to-one which takes a relation
;; and tells if it is one-to-one (two keys don't map to same second)
(define (one-to-one? rel)
  (set? (seconds rel)))


;; ch - Lambda the Ultimate

;; we write rember function again but insted of using eq?/equal?
;; we will parametrize the predicate that is used to remove member
;; and call it rember-f
;; test? predicate parameter takes two elements and returns true if second element is to removed from list
(define (rember-f test? a l)
  (cond [(null? l) '()]
        [(test? a (car l)) (rember-f test? a (cdr l))]
        [#t (cons (car l) (rember-f test? a (cdr l)))]
        ))

;; we know eq? is a function of two arguments
;; so write  a curried version of eq?

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

;; >>eq?-c
;; #<procedure (? a)>
;; >>(eq?-c '2)
;; #<procedure (? x)>
;; >>((eq?-c '2) '2)
;; #t

;; curried functions are usefule
(define eq?-salad
  (eq?-c 'salad))

;; (eq?-salad 'ok)
;; #f
;; (eq?-salad 'salad)
;; #t


;; now we write rember-f in curried form that takes predicate
;; and returns a rember like function that only takes a and l
(define rember-f-curried
  (lambda (test?)
    (lambda (a l)
      (cond [(null? l) '()]
            [(test? (car l) a) ((rember-f-curried test?) a (cdr l))]
            [#t (cons (car l) ((rember-f-curried test?) a (cdr l)))]
            ))))

;; write insert-L in curried form
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond [(null? l) '()]
            [(test? (car l) old) (cons new (cons old (cdr l)))]
            [#t (cons (car l) ((insertL-f test?) new old (cdr l)))]
            ))))
                                        ;
;; write insert-R in curried form

;; write insert-g that takes L/R as parameter and does appropriate insertion

;; write a function seqL that takes two atoms and a list
;; and produces a list in same sequence
(define (seqL a1 a2 l)
  (cons a1 (cons a2 l)))


;; seqR makes list with order of atoms reversed
(define (seqR a1 a2 l)
  (cons a2 (cons a1 l)))

;; write insert-g of one argument (either seqL/seqR) and return the function
;; which given -> new old l does appropriate insertL/insertR
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond [(null? l) '()]
            [(eq? (car l) old) (seq new old (cdr l))]
            [#t (cons (car l) ((insert-g seq) new old (cdr l)))]
            ))))

(define insertL-new (insert-g seqL))
(define insertR-new (insert-g seqR))

;; name lambdas when they get very big and have common usages

;; pg137 write multiRemberT , the predicate test? that comes
;; already has atom argument
(define (multiRemberT test? lat)
  (cond [(null? lat) '()]
        [(test? (car lat)) (multiRemberT test? (cdr lat))]
        [#t (cons (car lat) (multiRemberT test? (cdr lat)))]
        ))


;; a simple function which checks if second argument is null
(define a-friend
  (lambda (x y)
    (null? y)))


;; txt defined function
(define (multirember&co a lat col)
  (cond [(null? lat) (col '() '())]
        [(eq? (car lat) a) (multirember&co a
                                           (cdr lat)
                                           (lambda
                                               (newlat seen)
                                             (col newlat (cons (car lat) seen))))]
        [#t (multirember&co a
                            (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat) seen)))]
        ))

;; since col is the last thing done in any col, it is a tail transfer
;; essentially above fn looks at every atom of lat to see whether it is eq? to a
;; those that are not are added to ls1, and those that are equal are added to ls2
;; finally it does tail transfer of (f ls1 ls2)

;; the name cal is short for "collector"
;; a  collector is sometimes called continuation

;; try to follow execution of above function where
;; a is 'tuna
;; lat is '(tuna)
;; col is a-friend

;; try to follow execution of above function where
;; a is 'tuna
;; lat is '(and tuna)
;; col is a-friend

;; return length of first list
(define (last-friend x y)
  (length x))

;; try to do (mutlrember&co 'tuna '(strawberries tuna and swordfish) last-friend)


;; write function evens-only* which removes all odd numbers
;; from a list of nested lists

(define (evens-only* l)
  (cond [(null? l) '()]
        [(list? (car l)) (cons (evens-only* (car l)) (evens-only* (cdr l)))]
        [(even? (car l)) (cons (car l) (evens-only* (cdr l)))]
        [#t (evens-only* (cdr l))]
        ))

;; what we
;; are really doing by using collectors is pushing
;; future computation functions to be called at
;; a later time (with appropriate data captured in closures)
;; also they help you collect multiple values as arguments
;; also instead of returning value, you have to call continuation.
;; and initial continuation will decide what to do.
;;; simplest example of initial continuation would be identity function
;; that works like return.


;; ch9

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

;; pick returns nth item of given list


;; shift takes a pair
;; whose first component is a pair
;; and builds a pair by shifting the second part of the first component
;; into the second ccomponent

(define (shift pr)
  (make-pair (fst (fst pr))
         (make-pair (snd (first pr))
                (snd pr))))


;; txt defined weight* function

(define (weight* pora)
  (cond [(atom? pora) 1]
        [#t (+ (* (weight* (fst pora)) 2)
               (weight* (snd pora)))]
        ))

;; weight is recursively defined as 
;; an atom's weight is 1,
;; and a pair's weight is 2 * fst's weight + snd's weight

;; txt defined a-pair function
;; returns true only when list has exactly two S-expressions
(define (a-pair? x)
  (cond [(atom? x) #f]
        [(null? x) #f]
        [(null? (cdr x)) #f]
        [(null? (cdr (cdr x))) #t]
        [#t #f]
        ))

;; (a-pair? '(() ())) is true
;; (a-pair? '(1 2)) is true
;; (a-pair? '(() () ())) is false

;; txt defined function align : depends on a-pair and shift, make-pair, fst, snd
(define (align pora)
  (cond [(atom? pora) pora]
        [(a-pair? (fst pora)) (align (shift pora))]
        [#t (make-pair (fst pora) (align (snd pora)))]
        ))


