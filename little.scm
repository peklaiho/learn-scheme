;; The Little Schemer

;; Check if the argument is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;; Chapter 1: Toys

;; The Law of Car
;; The primitive car is defined only for non-empty lists.

;; The Law of Cdr
;; The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list.

;; The Law of Cons
;; The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.

;; The Law of Null?
;; The primitive null? is defined only for lists.

;; The Law of Eq?
;; The primitive eq? takes two arguments. Each must be a non-numeric atom.

;; Chapter 2: Do It Again

;; Check if the argument is a list of atoms
(define lat?
  (lambda (l)
    (cond
     [(null? l) #t]
     [(atom? (car l)) (lat? (cdr l))]
     [else #f])))

;; Check if a is member of lat
(define member?
  (lambda (a lat)
    (cond
     [(null? lat) #f]
     [(eq? (car lat) a) #t]
     [else (member? a (cdr lat))])))

;; The First Commandment (preliminary)
;; Always ask null? as the first question in expressing any function.

;; Chapter 3: Cons

;; The Second Commandment
;; Use cons to build lists.

;; Remove the first occurence of a from lat
(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat)
                 (rember a (cdr lat)))])))

;; Select first items from a list of lists
(define firsts
  (lambda (l)
    (cond
     [(null? l) '()]
     [else (cons (car (car l))
                 (firsts (cdr l)))])))

;; The Third Commandment
;; When building a list, describe the first typical element, and then cons it onto the natural recursion.

;; For firsts function:
;; Typical element: (car (car l))
;; Natural recursion: (firsts (cdr l))

(define seconds
  (lambda (l)
    (cond
     [(null? l) '()]
     [else (cons (car (cdr (car l)))
                 (seconds (cdr l)))])))

;; Insert new to the right of old in lat
(define insertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons old (cons new (cdr lat)))]
     [else (cons (car lat)
                 (insertR new old (cdr lat)))])))

;; Insert new to the left of old in lat
(define insertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new lat)]
     [else (cons (car lat)
                 (insertL new old (cdr lat)))])))

;; Substitute first occurence of old for new in lat
(define subst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new (cdr lat))]
     [else (cons (car lat)
                 (subst new old (cdr lat)))])))

;; Substitute first occurence of o1 or o2 for new in lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     [(null? lat) '()]
     [(or (eq? (car lat) o1) (eq? (car lat) o2))
      (cons new (cdr lat))]
     [else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))])))

;; Remove all occurences of a from lat
(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a)
      (multirember a (cdr lat))]
     [else
      (cons (car lat)
            (multirember a (cdr lat)))])))

;; Insert new to the right of all occurences of old in lat
(define multiinsertR
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons old (cons new (multiinsertR new old (cdr lat))))]
     [else (cons (car lat)
                 (multiinsertR new old (cdr lat)))])))

;; Insert new to the left of all occurences of old in lat
(define multiinsertL
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new (cons old (multiinsertL new old (cdr lat))))]
     [else (cons (car lat)
                 (multiinsertL new old (cdr lat)))])))

;; The Fourth Commandment (preliminary)
;; Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using cdr, test termination with null?.

;; Substitute all occurences of old with new in lat
(define multisubst
  (lambda (new old lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) old)
      (cons new (multisubst new old (cdr lat)))]
     [else (cons (car lat)
                 (multisubst new old (cdr lat)))])))

;; Chapter 4: Numbers

;; Note that numbers in these exercies are positive integers and zero. Negative or fractional numbers are not supported.

;; Add 1 to n
(define add1
  (lambda (n)
    (+ n 1)))

;; Subtract 1 from n
(define sub1
  (lambda (n)
    (- n 1)))

;; Add m to n
(define o+
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (add1 (o+ n (sub1 m)))])))

;; zero? for numbers is like null? for lists
;; add1 for numbers is like cons for lists

;; (o+ 2 3) = 1 + (o+ 2 2)
;;          = 1 + 1 + (o+ 2 1)
;;          = 1 + 1 + 1 + (o+ 2 0)
;;          = 1 + 1 + 1 + 2

;; (o+ 3 2) = 1 + (o+ 3 1)
;;          = 1 + 1 + (o+ 3 0)
;;          = 1 + 1 + 3

;; Subtract m from n
(define o-
  (lambda (n m)
    (cond
     [(zero? m) n]
     [else (sub1 (o- n (sub1 m)))])))

;; Tup is short for tuple, a list of numbers

;; Natural way to build numbers:
;; Use o+ in place of cons, because
;; o+ builds numbers like cons builds lists

;; Sum of all numbers in a tuple
(define addtup
  (lambda (tup)
    (cond
     [(null? tup) 0]
     [else (o+ (car tup)
               (addtup (cdr tup)))])))

;; The First Commandment (first revision)
;; When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about it: (zero? n) and else.

;; The Fourth Commandment (first revision)
;; Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
;; when using cdr, test termination with null?
;; when using sub1, test termination with zero?

;; Multiply n by m
(define x
  (lambda (n m)
    (cond
     [(zero? m) 0]
     [else (o+ n (x n (sub1 m)))])))

;; For example (x 12 3) works similar to a loop that iterates m (3) times and sums n (12) that many times with itself: 12 + 12 + 12 = 36

;; (x 12 3) = 12 + (x 12 2)
;;          = 12 + 12 + (x 12 1)
;;          = 12 + 12 + 12 + (x 12 0)
;;          = 12 + 12 + 12 + 0

;; 0 is the value for the terminal condition for because it does not affect the value of +:
;; n + 0 = n

;; The Fifth Commandment
;; When building a value with o+, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.
;; When building a value with x, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.
;; When building a value with cons, always consider () for the value of the terminating line.

;; Take each number from tup1 and tup2 and add them together
(define tup+
  (lambda (tup1 tup2)
    (cond
     [(null? tup1) tup2]
     [(null? tup2) tup1]
     [else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))])))

;; If either argument is shorter, then the other argument is returned as the terminating condition.

;; Is n greater than m?
(define o>
  (lambda (n m)
    (cond
     [(zero? n) #f]
     [(zero? m) #t]
     [else (o> (sub1 n) (sub1 m))])))

;; Is n less than m?
(define o<
  (lambda (n m)
    (cond
     [(zero? m) #f]
     [(zero? n) #t]
     [else (o< (sub1 n) (sub1 m))])))

;; Are numbers n and m equal?
(define o=
  (lambda (n m)
    (cond
     [(o> n m) #f]
     [(o< n m) #f]
     [else #t])))

;; Use = to test equality for numbers
;; Use eq? to test equality for other atoms

;; Raise n to the m'th power
(define ↑
  (lambda (n m)
    (cond
     [(zero? m) 1]
     [else (x n (↑ n (sub1 m)))])))

;; Divide n by m
(define o/
  (lambda (n m)
    (cond
     [(o< n m) 0]
     [else (add1 (o/ (o- n m) m))])))

;; (o/ 15 4) = 1 + (o/ 11 4)
;;           = 1 + 1 + (o/ 7 4)
;;           = 1 + 1 + 1 + (o/ 3 4)
;;           = 1 + 1 + 1 + 0

;; Pick n'th element from lat
(define pick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (car lat)]
     [else (pick (sub1 n) (cdr lat))])))

;; Remove the n'th element from lat
(define rempick
  (lambda (n lat)
    (cond
     [(zero? (sub1 n)) (cdr lat)]
     [else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))])))

;; Remove all numbers from lat
(define no-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(number? (car lat)) (no-nums (cdr lat))]
     [else (cons (car lat)
                 (no-nums (cdr lat)))])))

;; Remove all non-numbers from lat
(define all-nums
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(not (number? (car lat))) (all-nums (cdr lat))]
     [else (cons (car lat)
                 (all-nums (cdr lat)))])))

;; Compare numbers with = and others with eq?
(define eqan?
  (lambda (a1 a2)
    (cond
     [(and (number? a1) (number? a2)) (= a1 a2)]
     [(or (number? a1) (number? a2)) #f]
     [else (eq? a1 a2)])))

;; How many times a occurs in lat
(define occur
  (lambda (a lat)
    (cond
     [(null? lat) 0]
     [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
     [else (occur a (cdr lat))])))

;; Check if n is one
(define one?
  (lambda (n)
    (= n 1)))

;; Chapter 5: Stars

;; Star (*) functions recur with both car and cdr whenever the car is a list.

;; Recursively remove all occurences of a from l
(define rember*
  (lambda (a l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l))) (cons
                             (rember* a (car l))
                             (rember* a (cdr l)))]
     [(eq? (car l) a) (rember* a (cdr l))]
     [else (cons (car l) (rember* a (cdr l)))])))

;; Recursively insert new to the right of all occurences of old
(define insertR*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l)))
      (cons
       (insertR* new old (car l))
       (insertR* new old (cdr l)))]
     [(eq? (car l) old)
      (cons old (cons new (insertR* new old (cdr l))))]
     [else (cons (car l) (insertR* new old (cdr l)))])))

;; The First Commandment (final)
;; When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else.
;; When recurring on a number, n, ask two questions about it: (zero? n) and else.
;; When recurring on a list of S-expressions, l, ask three questions about it: (null? l), (atom? (car l)), and else.

;; The Fourth Commandment (final)
;; Always change at least one argument while recurring. When recurring on a list of atoms, lat, use (cdr lat). When recurring on a number, n, use (sub1 n). And when recurring on a list of S-expressions, l, use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true.
;; It must be changed to be closer to termination. The changing argument must be tested in the termination condition:
;; - when using cdr, test termination with null?
;; - when using sub1, test termination with zero?

;; Recursively count the number of occurences of a in l
(define occur*
  (lambda (a l)
    (cond
     [(null? l) 0]
     [(not (atom? (car l)))
      (o+ (occur* a (car l)) (occur* a (cdr l)))]
     [(eq? (car l) a) (add1 (occur* a (cdr l)))]
     [else (occur* a (cdr l))])))

;; Recursively substitute all occurences of old with new
(define subst*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l)))
      (cons (subst* new old (car l)) (subst* new old (cdr l)))]
     [(eq? (car l) old)
      (cons new (subst* new old (cdr l)))]
     [else
      (cons (car l) (subst* new old (cdr l)))])))

;; Recursively insert new to the left of all occurences of old
(define insertL*
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(not (atom? (car l)))
      (cons
       (insertL* new old (car l))
       (insertL* new old (cdr l)))]
     [(eq? (car l) old)
      (cons new (cons old (insertL* new old (cdr l))))]
     [else (cons (car l) (insertL* new old (cdr l)))])))

;; Recursively check if a is member of l
(define member*
  (lambda (a l)
    (cond
     [(null? l) #f]
     [(not (atom? (car l)))
      (or (member* a (car l))
          (member* a (cdr l)))]
     [(eq? (car l) a) #t]
     [else (member* a (cdr l))])))

;; Find the leftmost atom in non-empty list that does not contain the empty list
(define leftmost
  (lambda (l)
    (cond
     [(atom? (car l)) (car l)]
     [else (leftmost (car l))])))

;; Leftmost is not a *-function because in only recurs on car.

;; Check if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [(and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))]
     [(or (atom? (car l1)) (atom? (car l2))) #f]
     [else (and (eqlist? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))])))

;; Check if two S-expressions are equal
;; Use prefix because equal? is already defined in Scheme
(define my-equal?
  (lambda (s1 s2)
    (cond
     [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
     [(or (atom? s1) (atom? s2)) #f]
     [else (eqlist? s1 s2)])))

;; New version of eqlist? using my-equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
     [(and (null? l1) (null? l2)) #t]
     [(or (null? l1) (null? l2)) #f]
     [else (and (my-equal? (car l1) (car l2))
                (my-equal? (cdr l1) (cdr l2)))])))

;; The Sixth Commandment
;; Simplify only after the function is correct.

;; We can rewrite the rember function to work with any S-expressions rather than just atoms.

;; Remove S-expression s from list of S-expressions l
(define rember
  (lambda (s l)
    (cond
     [(null? l) '()]
     [(my-equal? (car l) s) (cdr l)]
     [else (cons (car l) (rember s (cdr l)))])))

;; Is rember a "star" function now? No, because rember recurs with the cdr of l only.

;; Chapter 6: Shadows

;; Arithmetic expression for the purposes of this chapter is composed of numbers and operators +, * and ↑.

;; For example: 3 + 4 x 5
;; We can represent it as a list: (3 + (4 x 5))

;; Check if the argument is an arithmetic expression
(define numbered?
  (lambda (aexp)
    (cond
     [(atom? aexp) (number? aexp)]
     [else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))])))

;; The above numbered? function is wrong (it does not correctly check if the argument is an arithmetic expression, it just sort of assumes it is).

;; Calculate the value of a numbered arithmetic expression
(define value
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (car (cdr nexp)) '+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp)))))]
     [(eq? (car (cdr nexp)) 'x)
      (x (value (car nexp))
         (value (car (cdr (cdr nexp)))))]
     [else
      (↑ (value (car nexp))
         (value (car (cdr (cdr nexp)))))])))

;; The Seventh Commandment
;; Recur on the subparts that are of the same nature:
;; - on the sublists of a list
;; - on the subexpressions of an arithmetic expression

;; Now try to represent arithmetic expressions in a way where the operator comes first (similar to Lisp): (+ 1 (x 2 3))

;; New version of value where operator comes first
(define value2
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (car nexp) '+)
      (o+ (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp)))))]
     [(eq? (car nexp) 'x)
      (x (value2 (car (cdr nexp)))
         (value2 (car (cdr (cdr nexp)))))]
     [else
      (↑ (value2 (car (cdr nexp)))
         (value2 (car (cdr (cdr nexp)))))])))

;; We can write help functions to read the subexpressions.

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

;; New version of value using help functions
(define value3
  (lambda (nexp)
    (cond
     [(atom? nexp) nexp]
     [(eq? (operator nexp) '+)
      (o+ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp)))]
     [(eq? (operator nexp) 'x)
      (x (value3 (1st-sub-exp nexp))
         (value3 (2nd-sub-exp nexp)))]
     [else
      (↑ (value3 (1st-sub-exp nexp))
         (value3 (2nd-sub-exp nexp)))])))

;; The Eight Commandment
;; Use help functions to abstract from representations.

;; Another representation for numbers:
;; zero: ()
;; one: (())
;; two: (() ())
;; three: (() () ())

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

;; This is like o+ for this representation of numbers
(define o++
  (lambda (n m)
    (cond
     [(sero? m) n]
     [else (edd1 (o++ n (zub1 m)))])))

;; Chapter 7: Friends

;; Set is where each item appears only once

;; Check if the argument is a set
(define set?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(member? (car lat) (cdr lat)) #f]
     [else (set? (cdr lat))])))

;; Make a set (remove duplicates)
(define makeset
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(member? (car lat) (cdr lat))
      (makeset (cdr lat))]
     [else (cons (car lat)
                 (makeset (cdr lat)))])))

;; Makeset using multirember
(define makeset2
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else
      (cons (car lat)
            (makeset2 (multirember
                       (car lat)
                       (cdr lat))))])))

;; Is set1 a subset of set2?
(define subset?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [(member? (car set1) set2)
      (subset? (cdr set1) set2)]
     [else #f])))

;; Check if two sets are equal (contain equal atoms)
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;; Check if sets intersect (at least one atom of set1 is in set2)
(define intersect?
  (lambda (set1 set2)
    (cond
     [(null? set1) #f]
     [(member? (car set1) set2) #t]
     [else (intersect? (cdr set1) set2)])))

;; Make set that contains items from both sets
(define intersect
  (lambda (set1 set2)
    (cond
     [(null? set1) '()]
     [(member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2))]
     [else (intersect (cdr set1) set2)])))

;; Take all items from either set
(define union
  (lambda (set1 set2)
    (cond
     [(null? set1) set2]
     [(member? (car set1) set2)
      (union (cdr set1) set2)]
     [else (cons (car set1)
                 (union (cdr set1) set2))])))

;; Find common atoms in non-empty list of sets
(define intersectall
  (lambda (l-set)
    (cond
     [(null? (cdr l-set)) (car l-set)]
     [else (intersect (car l-set)
                      (intersectall (cdr l-set)))])))

;; Check if argument is a pair (list with exactly two items)
(define a-pair?
  (lambda (x)
    (cond
     [(atom? x) #f]
     [(null? x) #f]
     [(null? (cdr x)) #f]
     [(null? (cdr (cdr x))) #t]
     [else #f])))

;; Helper functions to improve readablity.

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; Finite function, a list of pairs in which no first
;; element of any pair is the same as any other first element.
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;; Swap the first and second element of pairs in a list
(define revrel
  (lambda (rel)
    (cond
     [(null? rel) '()]
     [else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))])))

;; Reverse the items of a pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;; New version of revrel, using revpair
(define revrel
  (lambda (rel)
    (cond
     [(null? rel) '()]
     [else (cons (revpair (car rel))
                 (revrel (cdr rel)))])))

;; Check if the second element of each pair is unique
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;; Another way to write fullfun?
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

;; Chapter 8: Lambda

;; Version of rember that takes a comparison function
(define rember-f
  (lambda (test? a l)
    (cond
     [(null? l) '()]
     [(test? (car l) a) (cdr l)]
     [else (cons (car l)
                 (rember-f test? a (cdr l)))])))

;; Functions can return other functions, known as "currying"

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

;; Now rewrite rember-f as a function that takes one argument test? and returns a new function

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) '()]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l)
                   ((rember-f test?) a (cdr l)))]))))

;; We can make different versions of rember-f by passing different test? functions

(define rember-eq? (rember-f eq?))
(define rember-equal? (rember-f equal?))
(define rember-= (rember-f =))

;; We can transform insertL into insertL-f the same way

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(test? (car l) old)
        (cons new (cons old (cdr l)))]
       [else (cons (car l)
                   ((insertL-f test?) new old (cdr l)))]))))

;; ... and same for insertR-f

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(test? (car l) old)
        (cons old (cons new (cdr l)))]
       [else (cons (car l)
                   ((insertR-f test?) new old (cdr l)))]))))

;; Note that insertL-f and insertR-f are very similar: only one line differs
;; We can write small functions for only the different parts

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;; Now we can write generic function insert-g which can create insertL and insertR

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       [(null? l) '()]
       [(eq? (car l) old)
        (seq new old (cdr l))]
       [else (cons (car l)
                   ((insert-g seq) new old (cdr l)))]))))

(define insertL2 (insert-g seqL))
(define insertR2 (insert-g seqR))

;; However, it is not necessary to define seqL and seqR at all
;; We can just pass in their definition to insert-g

(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR2
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

;; Similarly we can re-define subst because only the middle part differs from insertL and insertR

(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

;; We have just seen the power of abstraction

;; The Ninth Commandment
;; Abstract common patterns with a new function.
