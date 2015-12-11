(define size 2)

(define my-list (lambda a a))

(define (uuu x) (* x x))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square x)))

;; 演習 1.2
(define 1-2 (/ (+ 4 5 (- 2 (- 3 (+ 6 (/ 4 5))))) (* (- 2 7) (- 6 2) 3)))

;; 演習 1.3
(define (big2 a b c)
  (cond ((<= a b c) (+ (square b) (square c)))
        ((<= b a c)) (+ (square a) (square c))
        (else (+ (square a) (square b)))))

;; 演習 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 演習 1.5
(define (p) (p))
