;; Take n elements from list without evaluating whole list.
;; Use continuation to exit early when enough items.
;;
;; We use 'append' instead of 'cons' to add new items at the end.
;; Optionally we could use 'cons' and then use 'reverse' to change the order.

(define take-n
  (lambda (ls n)
    (call/cc
     (lambda (break)
       (let loop ([l-in ls] [l-out '()])
         (if (or (null? l-in) (= (length l-out) n))
             (break l-out)
             (loop (cdr l-in) (append l-out (list (car l-in))))))))))

;; > (take-n '(1 2 3 4 5 6) 3)
;; (1 2 3)
;; > (take-n '(1 2) 3)
;; (1 2)

;; However, that was just for learning continuations. We can
;; actually write the function in a much shorter way like this:

(define take-n-2
  (lambda (ls n)
    (let loop ([ls ls] [i 0])
      (if (or (null? ls) (= i n))
          '()
          (cons (car ls) (loop (cdr ls) (+ i 1)))))))

;; > (take-n-2 '(1 2 3 4 5 6) 3)
;; (1 2 3)
;; > (take-n-2 '(1 2) 3)
;; (1 2)

;; However, this not properly tail recursive:

;; > ,trace (take-n-2 '[1 2 3 4 5] 3)
;; trace: (take-n-2 (1 2 3 4 5) 3)
;; trace: (loop (1 2 3 4 5) 0)
;; trace: |  (loop (2 3 4 5) 1)
;; trace: |  |  (loop (3 4 5) 2)
;; trace: |  |  |  (loop (4 5) 3)
;; trace: |  |  |  ()
;; trace: |  |  (3)
;; trace: |  (2 3)
;; trace: (1 2 3)

;; accumulator-passing style
;; use cons/reverse instead of append

(define take-n-aps
  (lambda (li lo i n)
    (if (or (null? li) (= i n)) lo
        (take-n-aps (cdr li) (cons (car li) lo ) (+ i 1) n))))

(define take-n-3
  (lambda (ls n)
    (reverse (take-n-aps ls '() 0 n))))

;; > ,trace (take-n-3 '(1 2 3 4 5) 3)
;; trace: (take-n-3 (1 2 3 4 5) 3)
;; trace: |  (take-n-aps (1 2 3 4 5) () 0 3)
;; trace: |  (take-n-aps (2 3 4 5) (1) 1 3)
;; trace: |  (take-n-aps (3 4 5) (2 1) 2 3)
;; trace: |  (take-n-aps (4 5) (3 2 1) 3 3)
;; trace: |  (3 2 1)
;; trace: (reverse (3 2 1))
;; trace: (1 2 3)
