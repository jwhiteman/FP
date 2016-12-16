;; using fold to define map & filter.

(define fold
  (lambda (fold-f acc l)
    (cond
      ((null? l) acc)
      (else
        (fold fold-f
              (fold-f acc (car l))
              (cdr l))))))

(define map
  (lambda (map-f l)
    (reverse
      (fold (lambda (acc e)
              (cons (map-f e) acc))
            '()
            l))))

(define filter
  (lambda (filter-f l)
    (reverse
      (fold (lambda (acc e)
              (cond
                ((filter-f e)
                 (cons e acc))
                (else acc)))
            '()
            l))))

; (map add1 '(1 2 3))
; (filter (lambda (n) (> n 3)) '(1 2 3 4 5))
