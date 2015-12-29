
(define (read-list)
  (let ((n (read)))
    (if (eof-object? n)
      '()
      (cons n (read-list)))))

(define (print-list lst)
  (if (null? lst)
    (display "")
    (begin (display (car lst))
           (newline)
           (print-list (cdr lst)))))

