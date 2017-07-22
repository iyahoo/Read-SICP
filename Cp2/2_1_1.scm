(use gauche.test)


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2.1

(define (make-rat n d)
  (let* ([abs-n (abs n)]
         [abs-d (abs d)]
         [g (gcd abs-n abs-d)]
         [sign (if (> (* n d) 0) 1 -1)])
    (cons (* sign (/ abs-n g)) (/ abs-d g))))

;; 2.2

(define (make-point x y)
  (cons x y))

(define (point-x point)
  (car point))

(define (point-y point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint point-f point1 point2)
  (/ (+ (point-f point1) (point-f point2)) 2))

(define (midpoint-segment seg)
  (let* ([start-point (start-segment seg)]
         [end-point   (end-segment seg)]
         [mid-x (midpoint point-x start-point end-point)]
         [mid-y (midpoint point-y start-point end-point)])
    (make-point mid-x mid-y)))

(define (make-rec point-a point-b)
  (make-segment point-a point-b))

(define (start-point rect)
  (start-segment rect))

(define (end-point rect)
  (end-segment rect))

(define (points-diff x-or-y p1 p2)
  (abs (- (x-or-y p1) (x-or-y p2))))

(define (periphery rect)
  (let ([sp (start-point rect)]
        [ep (end-point   rect)])
    (* 2 (+ (points-diff point-x sp ep) (points-diff point-y sp ep)))))

(define (area rect)
  (let ([sp (start-point rect)]
        [ep (end-point   rect)])
    (* (points-diff point-x sp ep) (points-diff point-y sp ep))))


(test-section "test 2.3")
(test "points-diff" 6
      (^[] (points-diff point-x (make-point 5 0) (make-point -1 0))))

(test "periphery" 6
      (^[] (periphery (make-rec (make-point 0 2) (make-point 1 0)))))

(test "area" 2
      (^[] (area (make-rec (make-point 0 2) (make-point 1 0)))))


;; 2.4

(define (cons- x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (cons- x y)
  (lambda (m)
    (cond ((equal? m :car) x)
          ((equal? m :cdr) y)
          (else (error "Argument not 0 or 1: CONS" m)))))

(define (cons-- x y)
  (lambda (m) (m x y)))

(define (car-- z)
  (z (lambda (p q) p)))

(define (cdr-- z)
  (z (lambda (p q) q)))


;; 2.5

(define (cons- x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-cdr pair base)
  (let loop [(pair pair) (count 0)]
    (if (= (modulo pair base) 0)
        (loop (/ pair base) (+ count 1))
        count)))

(define (car- pair)
  (car-cdr pair 2))

(define (cdr- pair)
  (car-cdr pair 3))


;; 2.6

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (plus p q)
  (lambda (f) (lambda (x) ((p f) ((q f) x)))))

(define (inc x)
  (+ 1 x))

(test-section "practice 2.6")

(test "zero" 0
      (^[] ((zero inc) 0)))

(test "one" 1
      (^[] ((one inc) 0)))

(test "two" 2
      (^[] ((two inc) 0)))

(test "add-1" 3
      (^[] (((add-1 (add-1 one)) inc) 0)))

(test "plus" 3
      (^[] (((plus two one) inc) 0)))

(test "plus" 5
      (^[] (((plus two three) inc) 0)))

;; 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; 練習 x2.7

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

