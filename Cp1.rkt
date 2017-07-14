(define size 2)

(define my-list (lambda a a))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; 練習問題 1.2
(define 1-2 (/ (+ 4 5 (- 2
                         (- 3
                            (+ 6
                               (/ 4 5)))))
               (* (- 2 7)
                  (- 6 2)
                  3)))

;; 練習問題 1.3
(define (1-3 a b c)
  (cond ((<= a b c) (sum-of-squares b c))
        ((<= b a c) (sum-of-squares a c))
        (else (sum-of-squares a b))))

;; 練習問題 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 練習問題 1.5
;; (define (p) (p))

;; 1.1.7

;; (define (sqrt- x)
;;   (define (sqrt-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (sqrt-iter (improve guess x) x)))
;;   (define (improve guess)
;;     (average guess (/ x guess)))
;;   (define (average x y)
;;     (/ (+ x y) 2))
;;   (define (good-enough? guess x)
;;     (< (abs (- (square guess) x)) 0.001))
;;   (sqrt-iter 1.0 x))

(define (test-sqrt x)
  (list (sqrt x) (sqrt- x)))

;; 練習問題 1.8

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

;; 練習問題 1.9

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (plus1 a b)
  (if (= a 0)
      b
      (inc (plus1 (dec a) b))))

(define (plus2 a b)
  (if (= a 0)
      b
      (plus2 (dec a) (inc b))))

;; 練習問題 1.10 (n が整数)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n)
  (A 0 n))

;;;; 2n 

(define (g n)
  (A 1 0))

;;;; 2^n (n >= 0)

(define (h n)
  (A 2 0))

;;;; h(n) = 2^h(n-1) (h(1) = 2)

;; 1.2.2

;; 金種 = 5

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 金種 = 2

(define (count-change2 amount)
  (cc amount 2))

;; 反復プロセスにするために
;; 下から行く?

;; 練習問題 1.11
;; f(n) = (f(n-1) + 2*f(n-2) + 3*f(n-3))
;;        (n (n < 3))

(define (f n)
  (f-rec 2 1 0 n))

(define (f-rec a b c n)
  (cond ((= 0 n) c)
        (else (f-rec (+ a (* 2 b) (* 3 c))
                     a
                     b
                     (- n 1)))))

;; 練習問題 1.12

(define (pascal-triangle row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal-triangle (- row 1) col)
                 (pascal-triangle (- row 1) (- col 1))))))

;; 1.2.4

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; 1.2.5

(define (as/gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

