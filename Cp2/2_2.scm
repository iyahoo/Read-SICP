(use gauche.test)

;; 階層データと閉包性

;; という意味で使います。それに対して、リスト構造 (list structure) という用語は、リス トに限らず、ペアによって作られた任意のデータ構造を指します。
;;
;; (Page 106).

(define (append- list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append- (cdr list1) list2))))

;; 練習問題 2.17

(define (last-pair lis)
  (if (null? (cdr lis))
      lis
      (last-pair (cdr lis))))

(test-section "last-pair")

(test* "" (list 1) (last-pair (list 1)))
(test* "" (list 3) (last-pair (list 1 2 3)))

;; 練習問題 2.18

(define (%reverse lis acm)
  (if (null? lis)
      acm
      (%reverse (cdr lis) (cons (car lis) acm))))

(define (reverse- lis)
  (%reverse lis (list)))

(test-section "reverse-")

(test* "" (list 3 2 1) (reverse- (list 1 2 3)))
(test* "" (list 1) (reverse- (list 1)))
(test* "" (list) (reverse- (list)))

(define (my-reverse items)
  (define (flatten l i)
    (if (null? l)
        (list i)
        (cons (car l) (flatten (cdr l) i))))
  (if (null? items)
      (list)
      (flatten (my-reverse (cdr items)) (car items))))

(test* "" '(1) (my-reverse '(1)))
(test* "" '(2 1) (my-reverse '(1 2)))
(test* "" '(3 2 1) (my-reverse '(1 2 3)))

;; 練習問題 2.19

(use util.combinations)

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 1 5 50 25 10))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(test-section "cc")

(test* "" 292 (cc 100 us-coins))


;; 練習問題 2.20

(define (same-parity . nums)
  (define (get-evens l)
    (cond [(null? l)
           l]
          [(even? (car l))
           (cons (car l) (get-evens (cdr l)))]
          [else
           (get-evens (cdr l))]))
  (define (get-odds l)
    (cond [(null? l)
           l]
          [(odd? (car l))
           (cons (car l) (get-odds (cdr l)))]
          [else
           (get-odds (cdr l))]))
  (if (even? (car nums))
      (get-evens nums)
      (get-odds nums)))

(test-section "same-parity")

(test* "" (list 1) (same-parity 1))
(test* "" (list 2) (same-parity 2))
(test* "" (list 2 4 6) (same-parity 2 3 4 5 6))
(test* "" (list 1 3 5) (same-parity 1 2 3 4 5 6))

;; 練習問題 2.21

(define (square-list1 items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(test-section "square-list")

(test* "" (list 1 4 9) (square-list1 '(1 2 3)))
(test* "" (list 1 1 1) (square-list1 '(1 1 1)))

(test* "" (list 1 4 9) (square-list2 '(1 2 3)))
(test* "" (list 1 1 1) (square-list2 '(1 1 1)))

;; 練習問題 2.22

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) (cons (square (car things)) answer))))
;;   (iter items nil))

;; nil に対して items の先頭から順に計算した結果を cons しているので、結果は逆になってしまう。
;; というのは answer の変化は nil -> (1) -> (4 1) となるからである。

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) (cons answer (square (car things))))))
;;   (iter items '()))

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items '()))

;; こちらも answer の変化の推移を追っていくとわかるが、nil -> (nil . 1) -> ((nil . 1) . 4) というようになるためである。

;; 練習問題 2.23

(define (my-for-each proc lis)
  (define (proc-iter l)
    (proc (car l))
    (my-for-each proc (cdr l)))
  (if (null? lis)
      1
      (proc-iter lis)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

;; 練習問題 2.24

(list 1 (list 2 (list 3 4)))

;; 練習問題 2.25

(test-section "2.25")

(test* "" 7 (car (cdaddr '(1 3 (5 7) 9))))
(test* "" 7 (caar '((7))))
(test* "" 7 (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))

;; 練習問題 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(test-section "2.26")

(test* "" '(1 2 3 4 5 6) (append x y))
(test* "" '((1 2 3) 4 5 6) (cons x y))
(test* "" '((1 2 3) (4 5 6)) (list x y))

;; 練習問題 2.27

;; (define (my-reverse lis)
(define (%reverse lis acm)
  (if (null? lis)
      acm
      (%reverse (cdr lis) (cons (car lis) acm))))
;;   (%reverse lis (list)))

(define (deep-reverse lis)
  (define (%reverse lis acm)
    (if (null? lis)
        acm
        (%reverse (cdr lis) (cons (car lis) acm))))
  (if (not (pair? lis))
      lis
      (map deep-reverse (%reverse lis '()))))

(test-section "2.27")

(test* "" '(1) (deep-reverse '(1)))
(test* "" '(3 2 1) (deep-reverse '(1 2 3)))
(test* "" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))
(test* "" '((1 2) ((4 3) (2 1))) (deep-reverse '(((1 2) (3 4)) (2 1))))

(test* "" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))
(test* "" '((4 (3 2)) 1) (deep-reverse '(1 ((2 3) 4))))

(test* "" '((2 1)) (deep-reverse '((1 2))))
(test* "" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))

;; 練習問題 2.28

(define (fringe tree)
  (cond ((not (pair? tree))
         tree)
        ((not (pair? (car tree)))
         (cons (car tree) (fringe (cdr tree))))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(test-section "fringe")

(test* "" '(1 2 3 4 5) (fringe '(1 2 (3) 4 5)))
(test* "" '(1 2 3 4 5) (fringe '((1) (2) (3) (4) (5))))
(test* "" '(1 2 3 4 5) (fringe '(1 (2 (3) 4) 5)))
(test* "" '(1 2 3 4 5) (fringe '(1 (2 (3) 4) 5)))

;; 練習問題 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b

(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

;; c

(define (get-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (= (get-torque (left-branch mobile)) (get-torque (right-branch mobile))))

;; d
;; 以下のように関数を変えてもテストはうまくいくので大丈夫っぽい

;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))

;; (define (right-branch mobile)
;;   (cdr mobile))

;; (define (branch-structure branch)
;;   (cdr branch))

(test-section "2.29")

(test* "left-branch" '(1 2 3) (left-branch (make-mobile '(1 2 3) '(4 5 6))))
(test* "right-branch" '(4 5 6) (right-branch (make-mobile '(1 2 3) '(4 5 6))))

(test* "branch-length" 3 (branch-length (make-branch 3 '(4 5 6))))
(test* "branch-structure" '(4 5 6) (branch-structure (make-branch 3 '(4 5 6))))

(test* "total-weight" 5
       (total-weight (make-mobile (make-branch 2 2) (make-branch 3 3))))

(test* "total-weight" 7
       (total-weight (make-mobile (make-branch 2
                                               (make-mobile (make-branch 1 2)
                                                            (make-branch 1 2)))
                                  (make-branch 3 3))))

(test* "total-weight"
       15
       (total-weight (make-mobile
                      (make-branch 3
                                   (make-mobile
                                    (make-branch 2 5)
                                    (make-branch 1 5)))
                      (make-branch 3 5))))

(test* "get-torque"
       35
       (get-torque (make-mobile 7
                                (make-mobile
                                 (make-branch 2 2)
                                 (make-branch 3 3)))))

(test* "get-torque"
       14
       (get-torque (make-branch 2
                                (make-mobile
                                 (make-branch 2
                                              (make-mobile
                                               (make-branch 1 2)
                                               (make-branch 1 2)))
                                 (make-branch 3 3)))))

(test* "get-torque"
       45
       (get-torque (make-branch 3
                                (make-mobile
                                 (make-branch 3
                                              (make-mobile
                                               (make-branch 2 5)
                                               (make-branch 1 5)))
                                 (make-branch 3 5)))))

(test* "balanced?"
       #t
       (balanced?
        (make-mobile
         (make-branch 3
                      (make-mobile
                       (make-branch 3
                                    (make-mobile
                                     (make-branch 2 5)
                                     (make-branch 1 5)))
                       (make-branch 3 5)))
         (make-mobile 9
                      (make-mobile
                       (make-branch 2 2)
                       (make-branch 3 3))))))

(test* "balanced?"
       #f
       (balanced?
        (make-mobile
         (make-branch 3
                      (make-mobile
                       (make-branch 3
                                    (make-mobile
                                     (make-branch 2 5)
                                     (make-branch 1 5)))
                       (make-branch 3 5)))
         (make-mobile 4
                      (make-mobile
                       (make-branch 2 2)
                       (make-branch 3 3))))))

(define test-b
  (make-mobile
   (make-branch 3 6)
   (make-branch 2
                (make-mobile
                 (make-branch 1 4)
                 (make-branch 1 5)))))

(test* "check-balanced"
       #t
       (balanced? test-b))


