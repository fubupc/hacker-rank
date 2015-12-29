
(define (lst-dedup lst)
  (define (dedup-iter deduped seq)
    (if (null? seq)
      deduped
      (let ((next (car seq))
            (rest (cdr seq)))
        (if (member next deduped)
          (dedup-iter deduped rest)
          (dedup-iter (append deduped (list next)) rest)))))
  (dedup-iter '() lst))

(define (string-dedup str)
  (list->string (lst-dedup (string->list str))))

(let ((str (symbol->string (read))))
  (display (string-dedup str)))


