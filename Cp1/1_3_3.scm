(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond ([positive? test-value]
                 (search f neg-point midpoint))
                ([negative? test-value]
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ([a-value (f a)]
        [b-value (f b)])
    (cond ([and (negative? a-value) (positive? b-value)]
           (search f a b))
          ([and (negative? b-value) (positive? a-value)]
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define count 0)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (set! count (+ count 1))
    (let ([next #?=(f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (^[y] (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (^[y] (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


;; 1.1.7 のふりかえり

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; 比較用

;(define (sqrt x)
;  (fixed-point (average-damp (^[y] (/ x y))) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; 練習問題 1.40

(define (cubic a b c)
  (^[x] (+ (cube x) (* a x x) (* b x) c)))

;; 練習問題 1.41

(define (inc x)
  (+ x 1))

(define (double f)
  (^[x] (f (f x))))

;; 1.42

(define (compose f g)
  (^[x] (f (g x))))

(define (compose f g :rest args)
  (^[:rest args] (f (apply g args))))

