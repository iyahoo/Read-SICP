(use gauche.test)

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

(define (div-interval- x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; 練習 2.7

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(test-section "practice 2.7")

(test "upper-bound" 3
      (^[] (upper-bound (make-interval 3 0))))

(test "lower-bound" 0
      (^[] (lower-bound (make-interval 3 0))))

;; 練習問題 2.8

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))

(test-section "practice 2.8")

(test "sub-interval"
      (cons 8 -2)
      (^[] (sub-interval (make-interval 10 3)
                         (make-interval 5 2))))

(test "sub-interval"
      '(100 . 0)
      (^[] (sub-interval (make-interval 100 25)
                         (make-interval 25 0))))

;; 2.9 slack

;; 2.10
;; Q. 何故 0 を跨ぐとだめなのかがわからない
;; A. 0 を含む場所で除算する場合、 0 は計算不可能な上、その周辺な値が最大にならなければないらないが、具体的な値は決定不可能
;;

(define (div-interval x y)
  (mul-interval
   x
   (if (>= (* (upper-bound y) (lower-bound y)) 0)
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y)))
       (error "ゼロを跨ぐ区間です"))))

(define (interval-type int)
  (cond [(and (> (lower-bound int) 0) (> (upper-bound int) 0))
         :positive]
        [(and (<= (lower-bound int) 0) (>= (upper-bound int) 0))
         :include-0]
        [(and (< (lower-bound int) 0) (< (upper-bound int) 0))
         :negative]))

(test-section "interval-type")

(test* "interval-type1"
       :positive
       (interval-type (make-interval 1 2)))

(test* "interval-type2"
       :include-0
       (interval-type (make-interval -1 1)))

(test* "interval-type3"
       :negative
       (interval-type (make-interval -3 -2)))

(test* "interval-type4"
       :include-0
       (interval-type (make-interval 0 1)))

(define-syntax three-type-interval-calc
  (syntax-rules ()
    ((_ type positive include-0 negative)
     (cond [(equal? type :positive)
            positive]
           [(equal? type :include-0)
            include-0]
           [(equal? type :negative)
            negative]))))

(define (new-mul-interval x y)
  (let ([x-type (interval-type x)]
        [y-type (interval-type y)])
    (cond [(equal? x-type :positive)
           (cond [(equal? y-type :positive)
                  (make-interval (* (lower-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y)))]
                 [(equal? y-type :include-0)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y)))]
                 [(equal? y-type :negative)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (upper-bound y)))])]
          [(equal? x-type :include-0)
           (cond [(equal? y-type :positive)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (upper-bound y)))]
                 [(equal? y-type :include-0)
                  (let ((p1 (* (lower-bound x) (lower-bound y)))
                        (p2 (* (lower-bound x) (upper-bound y)))
                        (p3 (* (upper-bound x) (lower-bound y)))
                        (p4 (* (upper-bound x) (upper-bound y))))
                    (make-interval (min p2 p3)
                                   (max p1 p4)))]
                 [(equal? y-type :negative)
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (lower-bound y)))])]
          [(equal? x-type :negative)
           (cond [(equal? y-type :positive)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (lower-bound y)))]
                 [(equal? y-type :include-0)
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y)))]
                 [(equal? y-type :negative)
                  (make-interval (* (upper-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y)))])])))

(define (new-mul-interval x y)
  (let ([x-type (interval-type x)]
        [y-type (interval-type y)])
    (three-type-interval-calc x-type
      (three-type-interval-calc y-type
        (make-interval (* (lower-bound x) (lower-bound y))
                       (* (upper-bound x) (upper-bound y)))
        (make-interval (* (upper-bound x) (lower-bound y))
                       (* (upper-bound x) (upper-bound y)))
        (make-interval (* (upper-bound x) (lower-bound y))
                       (* (lower-bound x) (upper-bound y))))
      (three-type-interval-calc y-type
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y)))
         (let ((p1 (* (lower-bound x) (lower-bound y)))
               (p2 (* (lower-bound x) (upper-bound y)))
               (p3 (* (upper-bound x) (lower-bound y)))
               (p4 (* (upper-bound x) (upper-bound y))))
           (make-interval (min p2 p3)
                          (max p1 p4)))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
      (three-type-interval-calc y-type
        (make-interval (* (lower-bound x) (upper-bound y))
                       (* (upper-bound x) (lower-bound y)))
        (make-interval (* (lower-bound x) (upper-bound y))
                       (* (lower-bound x) (lower-bound y)))
        (make-interval (* (upper-bound x) (upper-bound y))
                       (* (lower-bound x) (lower-bound y)))))))

(test-section "x+y+")
(test* "mul-interval"
       (make-interval 1 5)
       (mul-interval (make-interval 1 5) (make-interval 1 1)))
(test* "new-mul-interval"
       (make-interval 1 5)
       (new-mul-interval (make-interval 1 5) (make-interval 1 1)))

(test-section "x+y-")
(test* "mul-interval"
       (make-interval 1 5)
       (mul-interval (make-interval 1 5) (make-interval 1 1)))
(test* "new-mul-interval"
       (make-interval 1 5)
       (new-mul-interval (make-interval 1 5) (make-interval 1 1)))

(test-section "x+y+-")
(test* "mul-interval"
       (make-interval -5 5)
       (mul-interval (make-interval 1 5) (make-interval -1 1)))
(test* "new-mul-interval"
       (make-interval -5 5)
       (new-mul-interval (make-interval 1 5) (make-interval -1 1)))

(test-section "x-y+")
(test* "mul-interval"
       (make-interval -10 -1)
       (mul-interval (make-interval -5 -1) (make-interval 1 2)))
(test* "new-mul-interval"
       (make-interval -10 -1)
       (new-mul-interval (make-interval -5 -1) (make-interval 1 2)))

(test-section "x-y+-")
(test* "mul-interval"
       (make-interval -10 5)
       (mul-interval (make-interval -5 -1) (make-interval -1 2)))
(test* "new-mul-interval"
       (make-interval -10 5)
       (new-mul-interval (make-interval -5 -1) (make-interval -1 2)))

(test-section "x-y-")
(test* "mul-interval"
       (make-interval 1 10)
       (mul-interval (make-interval -5 -1) (make-interval -2 -1)))
(test* "new-mul-interval"
       (make-interval 1 10)
       (new-mul-interval (make-interval -5 -1) (make-interval -2 -1)))

(test-section "x+-y+")
(test* "mul-interval"
       (make-interval -10 10)
       (mul-interval (make-interval -5 5) (make-interval 1 2)))
(test* "new-mul-interval"
       (make-interval -10 10)
       (new-mul-interval (make-interval -5 5) (make-interval 1 2)))

(test-section "x+-y+-")
(test* "mul-interval"
       (make-interval 0 5)
       (mul-interval (make-interval 0 5) (make-interval 0 1)))
(test* "new-mul-interval"
       (make-interval 0 5)
       (new-mul-interval (make-interval 0 5) (make-interval 0 1)))
(test* "mul-interval"
       (make-interval -5 5)
       (mul-interval (make-interval 1 -1) (make-interval 5 -5)))
(test* "new-mul-interval"
       (make-interval -5 5)
       (new-mul-interval (make-interval 1 -1) (make-interval 5 -5)))

(test-section "x+-y-")
(test* "mul-interval"
       (make-interval -10 10)
       (mul-interval (make-interval -5 5) (make-interval -2 -1)))
(test* "mul-interval"
       (make-interval -10 10)
       (new-mul-interval (make-interval -5 5) (make-interval -2 -1)))

(let
    ([larger_zero_0 (make-interval 2 1)]
     [larger_zero_1 (make-interval 3 2)]
     [across_zero_0 (make-interval 2 -1)]
     [across_zero_1 (make-interval 3 -2)]
     [smaller_zero_0 (make-interval -3 -2)]
     [smaller_zero_1 (make-interval -4 -1)]
     )
  (test* "x.l > 0 and y.l > 0"
         (mul-interval larger_zero_0 larger_zero_1)
         (new-mul-interval larger_zero_0 larger_zero_1))
  (test* "x.l < 0, x.u > 0 and y.l > 0"
         (mul-interval across_zero_0 larger_zero_0)
         (new-mul-interval across_zero_0 larger_zero_0))
  (test* "same"
         (mul-interval across_zero_1 larger_zero_0)
         (new-mul-interval across_zero_1 larger_zero_0))
  (test* "x.u < 0 and y.l > 0"
         (mul-interval smaller_zero_0 larger_zero_0)
         (new-mul-interval smaller_zero_0 larger_zero_0))
  (test* "same"
         (mul-interval smaller_zero_1 larger_zero_1)
         (new-mul-interval smaller_zero_1 larger_zero_1))
  (test* "x.l > 0 and y.l < 0 , y.u > 0"
         (mul-interval larger_zero_0 across_zero_0)
         (new-mul-interval larger_zero_0 across_zero_0))
  (test* "same"
         (mul-interval larger_zero_1 across_zero_1)
         (new-mul-interval larger_zero_1 across_zero_1))
  (test* "y and x across 0"
         (mul-interval across_zero_0 across_zero_1)
         (new-mul-interval across_zero_0 across_zero_1))
  (test* "x.l < 0 and y across 0"
         (mul-interval smaller_zero_0 across_zero_0)
         (new-mul-interval smaller_zero_0 across_zero_0))
  (test* "same"
         (mul-interval smaller_zero_1 across_zero_1)
         (new-mul-interval smaller_zero_1 across_zero_1))
  (test* "x.l > 0 and y.u < 0"
         (mul-interval larger_zero_0 smaller_zero_0)
         (new-mul-interval larger_zero_0 smaller_zero_0))
  (test* "same"
         (mul-interval larger_zero_1 smaller_zero_1)
         (new-mul-interval larger_zero_1 smaller_zero_1))
  (test* "x across 0 and y.u < 0"
         (mul-interval across_zero_0 smaller_zero_0)
         (new-mul-interval across_zero_0 smaller_zero_0))
  (test* "same"
         (mul-interval across_zero_1 smaller_zero_1)
         (new-mul-interval across_zero_1 smaller_zero_1))
  (test* "x.u < 0 and y.u < 0"
         (mul-interval smaller_zero_0 smaller_zero_1)
         (new-mul-interval smaller_zero_0 smaller_zero_1)))