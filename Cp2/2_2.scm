;; File 2_2.scm
(define-module sicp.2.2
  (export-all))

(use gauche.test)

(define nil '())

;; 階層データと閉包性

;; という意味で使います。それに対して、リスト構造 (list structure) という用語は、リス トに限らず、ペアによって作られた任意のデータ構造を指します。
;;
;; (Page 106).

;; (define (my-list . elems)
;;   (let loop [(elems elems) (lst '())]
;;     (if (null? elems)
;;         (reverse lst)
;;         (loop (cdr elems) (cons (car elems) lst)))))

(define (append- list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append- (cdr list1) list2))))

;; 練習問題 2.17

(define (last-pair lis)
  (let ([cdr-lis (cdr lis)])
    (if (null? cdr-lis)
        lis
        (last-pair cdr-lis))))

(test-section "last-pair")

(test* "" (list 1) (last-pair (list 1)))
(test* "" (list 3) (last-pair (list 1 2 3)))

;; 練習問題 2.18

(define (my-reverse lis)
  (define (%reverse lis acm)
    (if (null? lis)
        acm
        (%reverse (cdr lis) (cons (car lis) acm))))
  (%reverse lis nil))

(test-section "my-reverse")

(test* "" (list 3 2 1) (my-reverse (list 1 2 3)))
(test* "" (list 1) (my-reverse (list 1)))
(test* "" nil (my-reverse nil))

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

(define us-coins (list 1 5 10 25 50))

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

;; old version

(define (count-change amount)
  (cc-old amount 5))

(define (first-denomination-old kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc-old amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-old amount (- kinds-of-coins 1))
                 (cc-old (- amount (first-denomination-old kinds-of-coins))
                         kinds-of-coins)))))

(test-section "cc-old")

(test* "1" 292 (count-change 100))


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

(test-section "ex 2.20")

(test* "same-parity1" (list 1) (same-parity 1))
(test* "same-parity2" (list 2) (same-parity 2))
(test* "same-parity3" (list 2 4 6) (same-parity 2 3 4 5 6))
(test* "same-parity4" (list 1 3 5) (same-parity 1 2 3 4 5 6))

;; 練習問題 2.21

(define (square-list1 items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (^x (* x x)) items))

(test-section "ex 2.21")

(test* "square-list1.1" (list 1 4 9) (square-list1 '(1 2 3)))
(test* "square-list1.2" (list 1 1 1) (square-list1 '(1 1 1)))

(test* "square-list2.1" (list 1 4 9) (square-list2 '(1 2 3)))
(test* "square-list2.2" (list 1 1 1) (square-list2 '(1 1 1)))

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
;;   (iter items nil))

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items nil))

;; こちらも answer の変化の推移を追っていくとわかるが、nil -> (nil . 1) -> ((nil . 1) . 4) というようになるためである。

;; 練習問題 2.23

(define (my-for-each proc lis)
  (define (proc-iter l)
    (proc (car l))
    (my-for-each proc (cdr l)))
  (if (not (null? lis))
      (proc-iter lis)))

;;

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

;; 練習問題 2.24

;; (list 1 (list 2 (list 3 4)))

(cons 1
      (cons (cons 2
                  (cons (cons 3
                              (cons 4 nil))
                        nil))
            nil))

(test* "" (list 1 (list 2 (list 3 4))) (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)))

;; 練習問題 2.25

(test-section "ex 2.25")

(test* "" 7 (car (cdaddr '(1 3 (5 7) 9))))
(test* "" 7 (caar '((7))))
(test* "" 7 (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))

;; 練習問題 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(test-section "ex 2.26")

(test* "" '(1 2 3 4 5 6) (append x y))
(test* "" '((1 2 3) 4 5 6) (cons x y))
(test* "" '((1 2 3) (4 5 6)) (list x y))

;; 練習問題 2.27

(define (my-reverse lis)
  (define (iter lis acm)
    (if (null? lis)
        acm
        (iter (cdr lis) (cons (car lis) acm))))
  (iter lis nil))

(define (deep-reverse lis)
  (if (not (pair? lis))
      lis
      (map deep-reverse (my-reverse lis))))

(test-section "ex 2.27")

(test* "deep-reverse1" '(1) (deep-reverse '(1)))
(test* "deep-reverse2" '(3 2 1) (deep-reverse '(1 2 3)))
(test* "deep-reverse3" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))
(test* "deep-reverse4" '((1 2) ((4 3) (2 1))) (deep-reverse '(((1 2) (3 4)) (2 1))))

(test* "deep-reverse5" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))
(test* "deep-reverse6" '((4 (3 2)) 1) (deep-reverse '(1 ((2 3) 4))))

(test* "deep-reverse7" '((2 1)) (deep-reverse '((1 2))))
(test* "deep-reverse8" '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))

;; 練習問題 2.28

(define (fringe tree)
  (cond ((not (pair? tree))
         tree)
        ((not (pair? (car tree)))
         (cons (car tree) (fringe (cdr tree))))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(test-section "ex 2.28")

(test* "fringe1" '(1 2 3 4 5) (fringe '(1 2 (3) 4 5)))
(test* "fringe2" '(1 2 3 4 5) (fringe '((1) (2) (3) (4) (5))))
(test* "fringe3" '(1 2 3 4 5) (fringe '(1 (2 (3) 4) 5)))
(test* "fringe4" '(1 2 3 4 5) (fringe '(1 (2 (3) 4) 5)))

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

(test-section "ex 2.29")

(test* "left-branch" '(1 2 3) (left-branch (make-mobile '(1 2 3) '(4 5 6))))
(test* "right-branch" '(4 5 6) (right-branch (make-mobile '(1 2 3) '(4 5 6))))

(test* "branch-length" 3 (branch-length (make-branch 3 '(4 5 6))))
(test* "branch-structure" '(4 5 6) (branch-structure (make-branch 3 '(4 5 6))))

;; b

(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(test* "total-weight" 5
       (total-weight (make-mobile (make-branch 2 2) (make-branch 3 3))))

(test* "total-weight" 7
       (total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                                              (make-branch 1 2)))
                                  (make-branch 3 3))))

(test* "total-weight"
       15
       (total-weight (make-mobile (make-branch 3
                                               (make-mobile (make-branch 2 5)
                                                            (make-branch 1 5)))
                                  (make-branch 3 5))))

;; c
;; branch を受けとって紐の長さと総重量の積を取る。
(define (get-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

;; 左右の
(define (balanced? mobile)
  (= (get-torque (left-branch mobile)) (get-torque (right-branch mobile))))

;;   |
;; -----
;; |   |
;; |   |
;; 2   |
;;     3
(test* "get-torque1"
       5
       (get-torque (make-branch 1
                                (make-mobile (make-branch 2 2)
                                             (make-branch 3 3)))))

(test* "balanced?1"
       #f
       (balanced? (make-mobile (make-branch 2 2)
                               (make-branch 3 3))))

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

;; d
;; 以下のように関数を変えてもテストはうまくいくので大丈夫っぽい

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(test* "total-weight-d" 5
       (total-weight (make-mobile (make-branch 2 2) (make-branch 3 3))))

(test* "total-weight-d"
       15
       (total-weight (make-mobile (make-branch 3
                                               (make-mobile (make-branch 2 5)
                                                            (make-branch 1 5)))
                                  (make-branch 3 5))))

(test* "get-torque1-d"
       5
       (get-torque (make-branch 1
                                (make-mobile (make-branch 2 2)
                                             (make-branch 3 3)))))
(test* "get-torque2-d"
       14
       (get-torque (make-branch 2
                                (make-mobile
                                 (make-branch 2
                                              (make-mobile
                                               (make-branch 1 2)
                                               (make-branch 1 2)))
                                 (make-branch 3 3)))))

;; 2.30

(define (square x)
  (* x x))

;; 高階関数を使わない版
(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree)) (square-tree1 (cdr tree))))))

;; 使う版
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree) (square-tree2 sub-tree) (square sub-tree)))
       tree))

(test-section "ex 2.30")

(test* "square-tree1" '(1 (4 (9 16) 25) (36 49)) (square-tree1 '(1 (2 (3 4) 5) (6 7))))
(test* "square-tree2" '(1 (4 (9 16) 25) (36 49)) (square-tree2 '(1 (2 (3 4) 5) (6 7))))

;; 練習問題 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree3 tree)
  (tree-map square tree))

(test-section "ex 2.31")

(test* "square-tree3.a" '(1 (4 (9 16) 25) (36 49)) (square-tree3 '(1 (2 (3 4) 5) (6 7))))
(test* "square-tree3.b" (square-tree1 '(1 (2 (3 4) 5) (6 7))) (square-tree3 '(1 (2 (3 4) 5) (6 7))))

;; 練習問題 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (e) (cons (car s) e))
                          rest)))))

(test-section "2.32")

(test* "" '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) (subsets '(1 2 3)))
(test* "" '(() (2) (1) (1 2)) (subsets '(1 2)))

;; rest の定義時に subsets は繰り返し呼ばれる。s が '() であるとき (subsets s) は '(()) になる。
;; ここから s が (3) の時の rest は '(()) となり、map で (car s) つまり 3 を rest の各要素に cons するとここでは (3) ができ、(append '(()) ((3))) となり '(() (3)) ができる。これが s が (2 3) であるときの rest に束縛される。そうなると map によって生成されるリストは rest のそれぞれの要素に 2 が cons されたものなので、'((2) (2 3)) よってここでの結果は '(() (3) (2) (2 3)) となるので、あとは 1 が追加されたものができ、 (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) となる。

;; 2017/10/6

(define (filter predicate sequence)
  (cond ((null? sequence)
         nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

;; 右結合
;; (accumulate + 0 '(1 2 3 4))
;; ↓
;; (+ 1 (+ 2 (+ 3 (+ 4 0))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(test* "" '(2 3 4 5 6 7) (enumerate-interval 2 7))

(define (enumerate-tree tree)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(test* "" '(1 2 3 4 5) (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons nil (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons nil (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(test* "" 225 (product-of-squares-of-odd-elements (list 1 2 3 4 5)))

;; ex 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(test-section "ex 2.33")
(test* "my-map1" '(2 4 6) (my-map (^[x] (* x 2)) '(1 2 3)))
(test* "my-map1" '(1 1 1) (my-map (^[x] 1) '(3 9 28)))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(test* "my-append1" '(1 2 3 4 5 6) (my-append '(1 2 3) '(4 5 6)))
(test* "my-append2" '(1 2 3) (my-append nil '(1 2 3)))
(test* "my-append3" '(1 2 3) (my-append '(1 2 3) nil))

(define (my-length sequence)
  (accumulate (^[x y] (+ 1 y)) 0 sequence))

(test* "my-length1" 3 (my-length '(1 2 3)))
(test* "my-length2" 0 (my-length nil))
(test* "my-length3" 5 (my-length '(3 6 9 12 15)))

;; ex 2.34

(test-section "ex 2.34")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

(test* "horner-eval" 79 (horner-eval 2 (list 1 3 0 5 0 1)))

;; ex 2.35

(define (count-leaves-acc t)
  (accumulate + 0 (map (^[x] 1) (enumerate-tree t))))

(define (depth-hima l)
  (if (null? l)
      0
      (if (number? l)
          1
          (+ (depth-hima (car l))
             (depth-hima (cdr l))))))

(define (count-leaves-acc-hima t)
  (accumulate + 0 (map depth-hima t)))

(test-section "ex 2.35")

(test* "count-leaves1" 4 (count-leaves-acc (cons (list 1 2) (list 3 4))))
(test* "count-leaves2" 8 (count-leaves-acc (list (cons (list 1 2) (list 3 4))
                                                 (cons (list 1 2) (list 3 4)))))
(test* "count-leaves3" 14 (count-leaves-acc (list (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))
                                                  (list 1 2) (list 1 2) (list 1 2))))

(test* "count-leaves1" 4 (count-leaves-acc-hima (cons (list 1 2) (list 3 4))))
(test* "count-leaves2" 8 (count-leaves-acc-hima (list (cons (list 1 2) (list 3 4))
                                                      (cons (list 1 2) (list 3 4)))))
(test* "count-leaves3" 14 (count-leaves-acc-hima (list (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))
                                                       (list 1 2) (list 1 2) (list 1 2))))


;; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate   op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(test* "accumulate-n" '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))

;; ex 2.37

(test-section "ex 2.37")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(test* "dot-product" 26 (dot-product '(1 2 3) '(3 4 5)))

(define (matrix-*-vector m v)
  (map (^[mc] (dot-product mc v)) m))

(test* "matrix-*-vector" '(14 32 50) (matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(1 2 3)))

(define (transpose mat)
  (accumulate-n cons nil mat))

(test* "transpose" '((1 4 7) (2 5 8) (3 6 9)) (transpose '((1 2 3) (4 5 6) (7 8 9))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (^[mc] (matrix-*-vector cols mc)) m)))

(test* "matrix-*-matrix" '((9 12) (24 33)) (matrix-*-matrix '((1 2) (4 5)) '((1 2) (4 5))))


;; 2017/12/7
;; ex 2.38

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define foldr accumulate)

(test-section "ex 2.37")

(test* "foldr1" 3/2 (foldr / 1 (list 1 2 3)))
(test* "foldl1" 1/6  (foldl / 1 (list 1 2 3)))
;; '(1 (2 (3 nil)))
(test* "foldr2" (list 1 (list 2 (list 3 nil))) (foldr list nil (list 1 2 3)))
;; '(((nil 1) 2) 3)
(test* "foldl2" (list (list (list nil 1) 2) 3) (foldl list nil (list 1 2 3)))

;; op が可換であること

;; ex 2.39

(define (reverse-r sequence)
  (foldr (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (foldl (lambda (x y) (cons y x)) nil sequence))

(test-section "ex 2.38")

(test* "reverse-r" '(3 2 1) (reverse-r '(1 2 3)))
(test* "reverse-l" '(3 2 1) (reverse-l '(1 2 3)))

;;

(use math.prime)

(define prime? small-prime?)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (new-remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (permutations s)
  (if (null? s) ; 集合は空か?
      (list nil) ; 空集合を持つ列
      (flatmap (^[x]
                 (map (^[p] (cons x p))
                      (permutations (new-remove x s))))
               s)))

;; ex 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(test-section "ex 2.40")

(test* "prime-sum-pairs" (prime-sum-pairs 5) (prime-sum-pairs2 5))

;; ex 2.41

(define (unique-triples n)
  (flatmap (^[i] (flatmap (^[j] (map (^[k] (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                          (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(test-section "ex 2.41")

(test* "unique-triples1" '((3 2 1)) (unique-triples 3))
(test* "unique-triples2" '((3 2 1) (4 2 1) (4 3 1) (4 3 2)) (unique-triples 4))
(test* "unique-triples3" '((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3)) (unique-triples 5))

(define (sum-is-s? triple s)
  (= s (apply + triple)))

(define (s-sum-triples n s)
  (filter (^[t] (sum-is-s? t s))
          (unique-triples n)))

(test* "unique-triples" '((4 3 1) (5 2 1)) (s-sum-triples 5 8))

;; ex 2.42

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (cons new-row k))))

(define (same-row? a b)
  (= (car a) (car b)))

(define (same-col? a b)
  (= (cdr a) (cdr b)))

(define (same-diagonal? a b)
  (= (+ (car a) (cdr a)) (+ (car b) (cdr b))))

(define (same-anti-diagonal? a b)
  (= (- (car a) (cdr a)) (- (car b) (cdr b))))

(define (hit? a b)
  (or (same-row? a b) (same-col? a b)
      (same-diagonal? a b) (same-anti-diagonal? a b)))

(define (safe? k positions)
  (let ([kth-queen (ref positions (- k 1))]
        [rest-queen (cdr (reverse positions))])
    (not (reduce (^[a b] (or a b)) #f
                 (map (^[a-queen] (hit? kth-queen a-queen))
                      rest-queen)))))

(define (queens board-size)
  (define (queen-cols k)
    ;;(print "hotte")
    (if (= k 0)
        (list empty-board)
        (filter (^[positions] (safe? k positions))
                (flatmap (^[rest-of-queens]
                           (map (^[new-row]
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(test-section "ex ex 2.42")

(test* "adjoin-position" '((1 . 1) (3 . 2) (2 . 3))
       (adjoin-position 2 3 '((1 . 1) (3 . 2))))
(test* "same-row?" #t
       (same-row? '(1 . 1) '(1 . 2)))
(test* "same-row?" #f
       (same-row? '(2 . 1) '(1 . 2)))
(test* "same-col?" #t
       (same-col? '(1 . 1) '(2 . 1)))
(test* "same-diagonal?" #t
       (same-diagonal? '(2 . 2) '(3 . 1)))
(test* "same-anti-diagonal?" #t
       (same-anti-diagonal? '(2 . 2) '(3 . 3)))
(test* "safe?" #t
       (safe? 1 '((1 . 1))))
(test* "safe?" #t
       (safe? 4 '((2 . 1) (4 . 2) (1 . 3) (3 . 4))))

;; P134 の例 (row(行), col(列))
(define sample-ans-queen
  '((3 . 1) (7 . 2) (2 . 3) (8 . 4) (5 . 5) (1 . 6) (4 . 7) (6 . 8)))

(test* "queens" #t
       (not (null? (member sample-ans-queen (queens 8)))))

;; ex 2.43

(use gauche.time)

(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (begin (print "hogee")
               (list empty-board))
        (filter (^[positions] (safe? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-position new-row k rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; Queen-cols が呼ばれる回数？
;; 通常の queens は queen
;;

;; k = 1 := 1
;; k = 2 := 6
;; k = 3 := 39
;; k = 4 := 340
;; k = 5 := 3905

;; http://community.schemewiki.org/?sicp-ex-2.43
;; これによると `board-size ^ board-size` で、直感的には正しそうなのだが、実際に `queen-cols` が呼ばれる回数と一致しない

;; リスト (enumerate-interval 1 board-size) のサイズ分 (queen-cols k)



;; ex 2.53

(test* "" (list 'a 'b 'c)
       '(a b c))

(test* "" (list (list 'george))
       '((george)))

(test* "" (cdr '((x1 x2) (y1 y2)))
       '((y1 y2)))

(test* "" (cadr '((x1 x2) (y1 y2)))
       '(y1 y2))

(test* "" (pair? (car '(a short list)))
       #f)

(test* "" (memq 'red '((red shoes) (blue socks)))
       #f)

(test* "" (memq 'red '(red shoes blue socks))
       '(red shoes blue socks))

;; ex 2.54

(define (equal-? obj1 obj2)
  (cond [(or (null? obj1) (null? obj2))
         (and (null? obj1) (null? obj2))]
        [(or (not (pair? obj1)) (not (pair? obj2)))
         (eq? obj1 obj2)]
        [else
         (and (equal-? (car obj1) (car obj2))
              (equal-? (cdr obj1) (cdr obj2)))]))

(test* "" (equal? '() '()) (equal-? '() '()))
(test* "" (equal? '(a) '()) (equal-? '(a) '()))
(test* "" (equal? '() '(a)) (equal-? '() '(a)))
(test* "" (equal? '(a) '(a)) (equal-? '(a) '(a)))
(test* "" (equal? '(a b) '(a)) (equal-? '(a b) '(a)))
(test* "" (equal? '(a b) '(a b)) (equal-? '(a b) '(a b)))

(test* "" (equal? '((a)) '((a))) (equal-? '((a)) '((a))))
(test* "" (equal? '(((a))) '((a))) (equal-? '(((a))) '((a))))

(test* "" (equal? '(a b (a)) '(a b a)) (equal-? '(a b (a)) '(a b a)))

(test* "" (equal? '(a b (a b)) '(a b (a b))) (equal-? '(a b (a b)) '(a b (a b))))

;; ex 2.55

''abracadabra

(quote (quote abracadabra))

