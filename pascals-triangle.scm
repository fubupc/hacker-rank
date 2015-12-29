
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (fact n)
  (fact-iter 1 1 n))

(define (tri-val row col)
  (/ (fact (- row 1))
     (* (fact (- col 1))
        (fact (- row col)))))

(define (tri-row row)
  (define (row-from-col col)
    (if (= col row)
      (begin (display 1) (newline))
      (begin (display (tri-val row col))
             (display " ")
             (row-from-col (+ col 1)))))
  (row-from-col 1))

(define (tri-ang n)
  (if (= n 1)
    (tri-row 1)
    (begin (tri-ang (- n 1))
           (tri-row n))))


