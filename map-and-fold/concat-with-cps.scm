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


(concat '(1 2 3) '(4 5 6))
