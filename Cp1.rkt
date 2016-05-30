(define size 2)

(define my-list (lambda a a))

(define (uuu x) (* x x))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; 演習 1.2
(define 1-2 (/ (+ 4 5 (- 2
                         (- 3
                            (+ 6
                               (/ 4 5)))))
               (* (- 2 7)
                  (- 6 2)
                  3)))

;; 演習 1.3
(define (1-3 a b c)
  (cond ((<= a b c) (sum-of-squares b c))
        ((<= b a c) (sum-of-squares a c))
        (else (sum-of-squares a b))))

;; 演習 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 演習 1.5
;; (define (p) (p))

;; 1.1.7

(define (sqrt- x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))  
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0 x))

(define (test-sqrt x)
  (list (sqrt x) (sqrt- x)))

;; 演習 1.8

(define (cube x)
  (* x x x))

(define (1-8-improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (1-8-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (1-8-sqrt-iter guess x)
  (if (1-8-good-enough? guess x)
      guess
      (1-8-sqrt-iter (1-8-improve guess x) x)))

(define (1-8-cube-root x)  
  (1-8-sqrt-iter 1.0 x))

