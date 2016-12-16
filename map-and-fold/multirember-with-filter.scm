(define mk-eq
  (lambda (x)
    (lambda (y)
      (eq? x y))))

(define filter
  (lambda (filter-f l)
    (letrec ((FILTER (lambda (l)
                       (cond
                         ((null? l) '())
                         ((filter-f (car l))
                          (FILTER (cdr l)))
                         (else
                           (cons (car l)
                                 (FILTER (cdr l))))))))
      (FILTER l))))

(define multirember
  (lambda (a lat)
    (filter (mk-eq a) lat)))

(multirember 'c '(a c d c))
