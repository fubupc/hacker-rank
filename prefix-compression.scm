
;; find LCP length
(define (LCP-len x y)
  (define (len-iter lcp-len xlen ylen)
    (cond ((or (= lcp-len xlen)
               (= lcp-len ylen)
               (not (equal? (string-ref x lcp-len)
                            (string-ref y lcp-len))))
           lcp-len)
          (else (len-iter (+ lcp-len 1) xlen ylen))))

  (len-iter 0 (string-length x) (string-length y)))

(define (print-line str)
  (display (string-length str))
  (display " ")
  (display str)
  (newline))


(let ((x (symbol->string (read)))
      (y (symbol->string (read))))
  (let ((len (LCP-len x y)))
    (print-line (substring x 0 len))
    (print-line (substring x len))
    (print-line (substring y len))))

