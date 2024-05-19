;; basic factorial (not tail recursive)
(define fact
  (lambda (n)
    (cond
     [(zero? n) 1]
     [else (* n (fact (- n 1)))])))

;; > ,trace (fact 5)
;; trace: (fact 5)
;; trace: |  (fact 4)
;; trace: |  |  (fact 3)
;; trace: |  |  |  (fact 2)
;; trace: |  |  |  |  (fact 1)
;; trace: |  |  |  |  |  (fact 0)
;; trace: |  |  |  |  |  1
;; trace: |  |  |  |  1
;; trace: |  |  |  2
;; trace: |  |  6
;; trace: |  24
;; trace: 120

;; Notice shape is like a pyramid (nested calls stored in stack)

;; accumulator-passing style (tail recursive)
(define fact-aps
  (lambda (n acc)
    (cond
     [(zero? n) acc]
     [else (fact-aps (- n 1) (* acc n))])))

;; we need to call fact-aps with initial acc value of 1
(define fact2
  (lambda (n) (fact-aps n 1)))

;; > ,trace (fact2 5)
;; trace: (fact2 5)
;; trace: (fact-aps 5 1)
;; trace: (fact-aps 4 5)
;; trace: (fact-aps 3 20)
;; trace: (fact-aps 2 60)
;; trace: (fact-aps 1 120)
;; trace: (fact-aps 0 120)
;; trace: 120

;; Notice shape is flat, not a pyramid like above

;; continuation-passing style (CPS)
(define fact-cps
  (lambda (n k)
    (cond
     [(zero? n) (k 1)]
     [else
      (fact-cps
       (- n 1)
       (lambda (v) (k (* v n))))])))

(define fact3
  (lambda (n) (fact-cps n (lambda (v) v))))

;; > ,trace (fact3 5)
;; trace: (fact3 5)
;; trace: (fact-cps 5 #<procedure 7f5520253168 at /home/pekka/code/scm/…>)
;; trace: (fact-cps 4 #<procedure 7f55224b7de0 at /home/pekka/code/scm/…>)
;; trace: (fact-cps 3 #<procedure 7f55223ead80 at /home/pekka/code/scm/…>)
;; trace: (fact-cps 2 #<procedure 7f5521e92f00 at /home/pekka/code/scm/…>)
;; trace: (fact-cps 1 #<procedure 7f5521e9fca0 at /home/pekka/code/scm/…>)
;; trace: (fact-cps 0 #<procedure 7f5521ecdea0 at /home/pekka/code/scm/…>)
;; trace: (_ 1)
;; trace: (_ 1)
;; trace: (_ 2)
;; trace: (_ 6)
;; trace: (_ 24)
;; trace: (_ 120)
;; trace: 120

;; Shape is again flat, not taking up space in stack.
