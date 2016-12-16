(define y2
  (lambda (c)
    ((lambda (h)
       (h h))
     (lambda (f)
       (c (lambda (x y)
            ((f f) x y)))))))

(define make-rember
  (y2 (lambda (f)
        (lambda (a l)
          (cond
            ((null? l) '())
            ((eq? (car l) a)
             (f a (cdr l)))
            (else
              (cons (car l)
                    (f a (cdr l)))))))))

(make-rember 'c '(a c d c))
