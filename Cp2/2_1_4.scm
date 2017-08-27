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

;; 2.11

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

(define-syntax match-interval-type
  (syntax-rules ()
    ((_ target (type1 case1) (type2 case2) (type3 case3))
     (let ([target-type (interval-type target)])
       (cond [(equal? target-type type1)
              case1]
             [(equal? target-type type2)
              case2]
             [(equal? target-type type3)
              case3])))))

(define (new-mul-interval x y)
  (match-interval-type x
    [:positive (match-interval-type y
                 [:positive
                  (make-interval (* (lower-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y)))]
                 [:include-0
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (upper-bound x) (upper-bound y)))]
                 [:negative
                  (make-interval (* (upper-bound x) (lower-bound y))
                                 (* (lower-bound x) (upper-bound y)))])]
    [:include-0 (match-interval-type y
                  [:positive
                   (make-interval (* (lower-bound x) (upper-bound y))
                                  (* (upper-bound x) (upper-bound y)))]
                  [:include-0
                   (let ((p1 (* (lower-bound x) (lower-bound y)))
                         (p2 (* (lower-bound x) (upper-bound y)))
                         (p3 (* (upper-bound x) (lower-bound y)))
                         (p4 (* (upper-bound x) (upper-bound y))))
                     (make-interval (min p2 p3)
                                    (max p1 p4)))]
                  [:negative
                   (make-interval (* (upper-bound x) (lower-bound y))
                                  (* (lower-bound x) (lower-bound y)))])]
    [:negative (match-interval-type y
                 [:positive
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (upper-bound x) (lower-bound y)))]
                 [:include-0
                  (make-interval (* (lower-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y)))]
                 [:negative
                  (make-interval (* (upper-bound x) (upper-bound y))
                                 (* (lower-bound x) (lower-bound y)))])]))

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

(let ([larger_zero_0 (make-interval 2 1)]
      [larger_zero_1 (make-interval 3 2)]
      [across_zero_0 (make-interval 2 -1)]
      [across_zero_1 (make-interval 3 -2)]
      [smaller_zero_0 (make-interval -3 -2)]
      [smaller_zero_1 (make-interval -4 -1)])
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

;; 本文

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; 練習問題 2.12

(define (make-center-percent c p)  
  (let ([error (/ p 100)])
    (make-interval (- c (* c error)) (+ c (* c error)))))

(test* "make-center-percent: 1"
       (make-interval 99 101)
       (make-center-percent 100 1))

(test* "make-center-percent: 2"
       (make-interval 95 105)
       (make-center-percent 100 5))

(define (percent i)
  (let* [(c (center i))
         (ub (upper-bound i))]
    (* (- (/ ub c) 1) 100)))

(define (percent b)
  (* (/ (width b) (center b)) 100))

(test* "percent: 1"
       5
       (percent (make-center-percent 100 5)))

(test* "percent: 2"
       1
       (percent (make-center-percent 100 1)))

(test* "percent: 3"
       100
       (percent (make-center-percent 100 100)))

(test* "percent: 4"
       0.1
       (percent (make-center-percent 100 0.1)))

;; 2.13

;; B1 -> a +- b%, 0<=b<=1, [a*(1-b), a*(1+b)]
;; B2 -> c +- d%, 0<=d<=1, [c*(1-d), c*(1+d)]

;; B1 * B2 = [ac*(1-b)(1-d), ac*(1+b)(1+d)]

;;   ac*(1-b)(1-d)
;; = ac*(1 - (b+d) + bd) then bd is 0
;; = ac*(1 - (b+d))

;;   ac*(1+b)(1+d)
;; = ac*(1 + (b+d) + bd) then bd is 0
;; = ac*(1 + (b+d))

;; B1 * B2 = [ac(1-(b+d)), ac*(1+(b+d))]
;; center:  ac
;; width: (b+d)
;; percent: (b+d)/ac
