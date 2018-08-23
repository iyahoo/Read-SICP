;; 2.2.4 図形言語 (Page 136).

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(paint diagonal-shading)

(define painter '("△▽" "▽△"))

(define (print-painter str)
  (map (^[line] (print line)) str))

;; (define (beside w1 w2)
;;   ())

(define wave2
  (beside wave (flip-vert wave)))

(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left) (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
