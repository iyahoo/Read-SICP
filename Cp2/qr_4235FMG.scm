(use gauche.test)

;; P 157

(define (variable? e))
(define (same-variable? v1 v2))
(define (sum? e))

(define (addend e))
(define (augend e))
(define (make-sum a1 a2))
(define (product? e))
(define (multiplier e))
(define (multiplicand e))
(define (make-product m1 m2))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))