(define the-blank-char "_")
(define the-point-char "1")

(define the-empty-row '())
(define (empty-row? row) (eq? the-empty-row row))

(define the-empty-shape '())
(define (empty-shape? square) (eq? the-empty-shape square))

(define (first-row shape) (car shape))
(define (rest-rows shape) (cdr shape))

;; row 
(define (make-row char width)
  (if (= width 0)
    the-empty-row
    (cons char (make-row char (- width 1)))))

(define (join-row left right) (append left right))

(define (revert-row row)
  (define (iter result rest)
      (if (empty-row? rest)
        result
        (iter (cons (car rest) result) (cdr rest))))
  (iter '() row))

;; shape 
(define (append-row shape row)
  (if (empty-shape? shape)
    (list row)
    (cons (first-row shape)
          (append-row (rest-rows shape) row))))

(define (prepend-row shape row)
  (cons row shape))

(define (join-shape-horiz shape1 shape2)
  (if (empty-shape? shape1)
    the-empty-shape
    (prepend-row (join-shape-horiz (rest-rows shape1)
                                   (rest-rows shape2))
                 (join-row (first-row shape1)
                           (first-row shape2)))))

(define (join-shape-vert shape1 shape2) (append shape1 shape2))

;; print 
(define (print-row row)
  (if (empty-row? row)
    (display "\n")
    (begin (display (car row))
           (print-row (cdr row)))))

(define (print-shape shape)
  (if (empty-shape? shape)
    (display "")
    (begin (print-row (first-row shape))
           (print-shape (rest-rows shape)))))

;; square
(define (make-square char width height)
  (if (= height 0)
    the-empty-shape
    (prepend-row (make-square char width (- height 1))
                 (make-row char width))))


(define (square-width square) (length (first-row square)))
(define (square-height square) (length square))


;; triangle
(define (make-triangle char size)
  (define (iter triangle nth-row)
    (if (> nth-row size)
      triangle
      (iter (append-row triangle (make-row char nth-row))
            (+ nth-row 1))))
  (iter the-empty-shape 1))


(define (flip-vert shape)
  (if (empty-shape? shape)
    the-empty-shape
    (append-row (flip-vert (rest-rows shape))
                (first-row shape))))

(define (flip-horiz shape)
  (if (empty-shape? shape)
    the-empty-shape
    (prepend-row (flip-horiz (rest-rows shape))
                 (revert-row (first-row shape)))))

(define (padding square top right bottom left)
  (let ((width (square-width square))
        (height (square-height square)))
    (let ((top-padding (make-square the-blank-char (+ width right left) top))
          (bottom-padding (make-square the-blank-char (+ width right left) bottom))
          (left-padding (make-square the-blank-char left height))
          (right-padding (make-square the-blank-char right height)))
      (join-shape-vert
        top-padding
        (join-shape-vert
          (join-shape-horiz
            left-padding
            (join-shape-horiz
              square
              right-padding))
          bottom-padding)))))

(define (make-backslash-square size)
  (let ((left-padding (prepend-row (make-triangle the-blank-char (- size 1))
                                   the-empty-row)))
    (join-shape-horiz
      (join-shape-horiz left-padding (make-square the-point-char 1 size))
      (flip-vert left-padding))))


(define (make-Y-shape size)
  (let ((backslash (make-backslash-square size)))
    (let ((slash (flip-vert backslash)))
      (let ((top (join-shape-horiz
                   backslash
                   (padding slash 0 0 0 1))))
        (padding
          (join-shape-vert
            top
            (padding
              (make-square the-point-char 1 size)
              0 size 0 size))
          0 (- size 1) 0 (- size 1))))))

(define (make-Y-fractal size iter-count)
  (cond ((= iter-count 1)
         (padding (make-Y-shape size)
                  (- (* 2 size) 2) 0 0 0))
        ((= size 1)
         (make-Y-shape 1))
        (else
          (let ((child-fractal (make-Y-fractal (/ size 2) (- iter-count 1))))
            (join-shape-vert
              (join-shape-horiz (padding child-fractal 0 1 0 0) child-fractal)
              (make-Y-shape size))))))

(define (fractal-wrapper n)
  (padding (make-Y-fractal 16 n) 1 19 0 18))


(let ((n (read)))
  (print-shape (fractal-wrapper n)))

