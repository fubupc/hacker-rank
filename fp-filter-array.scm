
(define (my-filter lst fn)
  (cond ((null? lst) '())
        ((fn (car lst))
         (cons (car lst)
               (my-filter (cdr lst) fn)))
        (else (my-filter (cdr lst) fn))))

(define (f delim lst)
  (my-filter lst (lambda (n) (< n delim))))

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

(let ((upper (read))
      (lst (read-list)))
  (print-list (f upper lst)))

