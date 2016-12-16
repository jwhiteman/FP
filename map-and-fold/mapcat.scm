(define identity
  (lambda (x) x))

(define concat
  (lambda (l1 l2)
    (concat-helper l1 l2 identity)))

(define concat-helper
  (lambda (l1 l2 acc-f)
    (cond
      ((null? l1) (acc-f l2))
      (else
        (concat-helper (cdr l1)
                       l2
                       (lambda (cont)
                         (acc-f (cons (car l1) cont))))))))


;(concat '(1 2 3) '(4 5 6))

(define map
  (lambda (map-f l)
    (cond
      ((null? l) '())
      (else
        (cons (map-f (car l))
              (map map-f (cdr l)))))))

(define fold
  (lambda (fold-f acc l)
    (cond
      ((null? l) acc)
      (else
        (fold fold-f
              (fold-f acc (car l))
              (cdr l))))))


(define mapcat
  (lambda (map-f lol)
    (fold (lambda (acc l)
            (concat acc (map map-f l)))
          '()
          lol)))

(mapcat add1 '((1 2 3) (4 5 6)))
