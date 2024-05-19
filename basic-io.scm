(use-modules (ice-9 rdelim))

(define read-lines-from-port
  (lambda (port ls)
    (let ([line (read-line port 'split)])
      (cond
       [(eof-object? (cdr line)) ls]
       [else (read-lines-from-port port (cons (car line) ls))]))))

(define read-file-lines
  (lambda (filename)
    (call-with-input-file filename
      (lambda (port)
        (reverse (read-lines-from-port port '()))))))

