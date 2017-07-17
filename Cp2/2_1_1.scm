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


(test-section "test code")
(test "points-diff" 6
      (^[] (points-diff point-x (make-point 5 0) (make-point -1 0))))

(test "periphery" 6
      (^[] (periphery (make-rec (make-point 0 2) (make-point 1 0)))))

(test "area " 2
      (^[] (area (make-rec (make-point 0 2) (make-point 1 0)))))
