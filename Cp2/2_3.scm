(use gauche.test)

;; P 157

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0)
         a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;; 2.56

;; より多くの種類の式を扱えるようにこの基本的な 微分プログラムを拡張するにはどうすればよいかを⽰せ。
;; deriv の cond 式に、新しく扱いたい形

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation a1 a2)
  (cond [(=number? a2 0)
         1]
        [(=number? a1 0)
         0]
        [(=number? a2 1)
         a1]
        [(and (number? a1) (number? a2))
         (expt a1 a2)]
        [else (list '** a1 a2)]))

(define (deriv exp var)
  #?=exp
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp) 1))
                                     (deriv (base exp)
                                            var))))
        (else
         (error "unknown expression type: DERIV" exp))))

;; (test "" '(5 * y)
;;       (deriv '(5 * y * (x + 2)) 'x))

(test-section "ex 2.56")

(test* "make-exponentiation"
       (make-exponentiation 0 0)
       1)

(test* "make-exponentiation"
       (make-exponentiation 0 1)
       0)

(test* "make-exponentiation"
       (make-exponentiation 1 0)
       1)

(test* "make-exponentiation"
       (make-exponentiation 1 1)
       1)

(test* "make-exponentiation"
       'x
       (make-exponentiation 'x 1))

(test* "deriv"
       '(* 3 (* 2 x))
       (deriv '(* 3 (** x 2)) 'x))

;; ex 2.57

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cddr p))))

(define (exponent e)
  (if (= (length e) 3)
      (caddr e)
      (cons '** (cddr e))))

(test-section "ex 2.57")

(test* "augend"
       '(+ 1 x)
       (augend '(+ 1 1 x)))

(test* "augend"
       '(+ 1 x y)
       (augend '(+ 2 1 x y)))

(test* "multiplicand"
       '(* 2 x)
       (multiplicand '(* 1 2 x)))

(test* "exponent"
       '(** x y)
       (exponent '(** 3 x y)))






;; ex 2.58

(test-section "ex 2.58")

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(test* "sum?" #t (sum? '(1 + 2)))
(test* "sum?" #f (sum? '(+ 1 2)))

(define (addend s)
  (car s))

(test* "addend" 1 (addend '(1 + 2)))

(define (augend s)
  (caddr s))

(test* "augend" 2 (augend '(1 + 2)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0)
         a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2))
         (+ a1 a2)]
        [else (list a1 '+ a2)]))

(test* "make-sum" '(1 + x) (make-sum 1 'x))
(test* "make-sum" 2 (make-sum 1 1))
(test* "make-sum" 1 (make-sum 1 0))
(test* "make-sum" '(x + y) (make-sum 'x 'y))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(test* "product?" #t (product? '(1 * 2)))
(test* "product?" #f (product? '(* 1 2)))

(define (multiplier p)
  (car p))

(test* "multiplier" 1 (multiplier '(1 * x)))

(define (multiplicand p)
  (caddr p))

(test* "multiplicand" 'x
       (multiplicand '(1 * x)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(test* "make-sum" '(2 * x) (make-product 2 'x))
(test* "make-sum" 1 (make-product 1 1))
(test* "make-sum" 0 (make-product 1 0))
(test* "make-sum" '(x * y) (make-product 'x 'y))

(test* "deriv" 1
       (deriv '(1 + x) 'x))

(test* "deriv" 'y
       (deriv '(x * y) 'x))

(test* "deriv" '(3 + y)
       (deriv '((3 * x) + (y * x)) 'x))

(test* "deriv" 'x
       (deriv '((3 * x) + (y * x)) 'y))

;; ex 2.58 - b

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cddr s)))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cddr p)))

(test* "deriv" 1
       (deriv '(1 + x) 'x))

(test* "deriv" 'y
       (deriv '(x * y) 'x))

(test* "deriv" '(3 + y)
       (deriv '((3 * x) + (y * x)) 'x))

(test* "deriv" 'x
       (deriv '((3 * x) + (y * x)) 'y))

(test* "deriv" '0
       (deriv '(1 + 1 + 1) 'x))

(test* "deriv" '1
       (deriv '(1 * x + y * 1) 'x))

(test* "deriv" '((x * y) + (y * (x + 3)))
       (deriv '(x * y * (x + 3)) 'x))

(test* "" 4
       (deriv '(x + 3 * (x + y + 2)) 'x))

(test* "" 'y
       (deriv '(y * (x + 2)) 'x))

(test* "" '(5 * y)
       (deriv '(5 * y * (x + 2)) 'x))

(define (sum? s)
  (bin-expr-of? '+ s))

(define (product? s)
  (bin-expr-of? '* s))

(define (bin-expr-of? op expr)
  (and
   (pair? expr)
   (pair? (cdr expr))
   (or
    (eq? (cadr expr) op)
    (bin-expr-of? op (cddr expr)))))

;; (test* "" 2
;;        (deriv '(5 * y + 5 * y + (x * 2)) 'x))

;; (test* "" 5
;;        (deriv '(x * 2 + x * 2 + x + 2) 'x))



;; 2.3.3 例: 集合を表現する
;; （ページ162).

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

;; ex 2.59

(define (union-set set1 set2)
  (cond
   ((and (null? set1) (null? set2))
    '())
   ((null? set1)
    set2)
   ((null? set2)
    set1)
   ((element-of-set? (car set1) set2)
    (union-set (cdr set1) set2))
   (else
    (cons (car set1) (union-set (cdr set1) set2)))))

;; ex 2.60

(define (ns-adjoin-set x set)
  (cons x set))

(define (ns-intersection-set set1 set2)
  (intersection-set set1 set2))

(define (ns-union-set set1 set2)
  (append set1 set2))


;;

(define (o-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))



(define (o-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


;; ex 2.61

(define (o-adjoin-set x set)
  (cond ((null? set)
         (cons x '()))
        (else
         (let ((x1 (car set)))
           (cond
            ((= x x1)
             set)
            ((< x x1)
             (cons x set))
            (else
             (cons x1 (o-adjoin-set x (cdr set)))))))))


(test* "o-adjoin-set" '(4) (o-adjoin-set 4 '()))
(test* "o-adjoin-set" '(0 1 2 3) (o-adjoin-set 0 '(1 2 3)))
(test* "o-adjoin-set" '(1 2 3 4) (o-adjoin-set 4 '(1 2 3)))
(test* "o-adjoin-set" '(1 2 3 4) (o-adjoin-set 3 '(1 2 4)))


;; ex 2.62

(define (o-union-set set1 set2)
  (cond
   ((and (null? set1) (null? set2))
    '())
   ((null? set1)
    set2)
   ((null? set2)
    set1)
   (else
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
       ((= x1 x2)
        (o-union-set (cdr set1) set2))
       ((< x1 x2)
        (cons x1 (o-union-set (cdr set1) set2)))
       (else
        (cons x2 (o-union-set set1 (cdr set2)))))))))

(test* "o-union-set" '(4) (o-union-set '(4) '()))
(test* "o-union-set" '(3) (o-union-set '() '(3)))
(test* "o-union-set" '(0 1 2 3) (o-union-set '(0 2) '(1 2 3)))
(test* "o-union-set" '(1 2 3 4 5) (o-union-set '(4 5) '(1 2 3)))
(test* "o-union-set" '(0 1 2 4 6) (o-union-set '(0 6) '(1 2 4)))
(test* "o-union-set" '(1 2 4 5 6) (o-union-set '(5 6) '(1 2 4)))
(test* "o-union-set" '(0 1 2 3) (o-union-set '(0 1) '(1 2 3)))
