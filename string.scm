;; Some string operations that demonstrate the use of
;; call/cc to exit early when the searched character is found.

;; Find the first occurence of character from string.
;; Returns the index of the character or #f if not found.
;; Start is the index to search from.
(define string-first-char
  (lambda (str ch start)
    (call/cc
     (lambda (break)
       (let loop ([i start] [len (string-length str)])
         (cond
          [(>= i len) (break #f)]
          [(eqv? (string-ref str i) ch) (break i)]
          [else (loop (+ i 1) len)]))))))

;; Find the last occurence of character from string.
;; Returns the index of the character or #f if not found.
;; Start is the index to search from.
(define string-last-char
  (lambda (str ch start)
    (call/cc
     (lambda (break)
       (let loop ([i start])
         (cond
          [(< i 0) (break #f)]
          [(eqv? (string-ref str i) ch) (break i)]
          [else (loop (- i 1))]))))))

;; string-first-char and string-last-char are properly tail recursive!

;; Split string by character.
;; Delimiter characters are not included in results.
;; Returns a list that contains always at least one string:
;; If no delimiters are found, the full original string is returned.
(define string-split
  (lambda (str ch)
    (let loop ([start (string-last-char str ch (- (string-length str) 1))]
               [end (string-length str)]
               [results '()])
      (if (not start) (cons (substring str 0 end) results)
          (loop (string-last-char str ch (- start 1)) start
                (cons (substring str (+ start 1) end) results))))))
