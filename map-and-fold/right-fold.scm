;; notes fold takes a binary function

(define identity
  (lambda (x) x))

;; polya: solve something easier first.
(define adder
  (lambda (l)
    (adder-helper 0 l identity)))

(define adder-helper
  (lambda (final l cont-f)
    (cond
      ((null? l) (cont-f final))
      (else
        (adder-helper final
                      (cdr l)
                      (lambda (acc)
                        (cont-f (+ acc (car l)))))))))

(define fold-right
  (lambda (fold-f acc l)
    (fold-right-helper fold-f acc l identity)))

(define fold-right-helper
  (lambda (fold-f terminating-value l cont-f)
    (cond
      ((null? l) (cont-f terminating-value))
      (else
        (fold-right-helper fold-f
                           terminating-value
                           (cdr l)
                           (lambda (acc)
                             (cont-f (fold-f acc (car l)))))))))

(fold-right (lambda (acc e)
              (begin
                (display e)
                (display "\n"))
              (+ acc e))
            0
            '(1 2 3))

;; CPS sets up collections to be folded from the right; you pass in the resulting matryoshka doll function the terminating
;; value (eg 0, or '() etc) and it essentially right folds its way back to the the first element and finally the
;; core-func
