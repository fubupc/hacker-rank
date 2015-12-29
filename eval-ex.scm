(define (fact n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(define (pow b n)
  (if (= n 0)
    1
    (* b (pow b (- n 1)))))

(define (nth-term x n)
  (/ (pow x (- n 1))
     (fact (- n 1))))


(define (ex x)
  (define (terms-sum num)
    (if (= num 0)
      0
      (+ (nth-term x num)
         (terms-sum (- num 1)))))

  (terms-sum 10))



(define (input-to-list n)
  (if (= n 0)
    '()
    (cons (read) (input-to-list (- n 1)))))

(let ((n (read)))
  (let ((nums (input-to-list n)))
    (display nums)
    (map (lambda (num)
           (display (ex num))
           (newline))
         nums)
    '(void)))
