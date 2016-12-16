;; Write `filter` 3 different ways.

;; helpers
(define greater-than-two (lambda (n) (> n 2)))
(define fold (lambda (fun acc l) (cond ((null? l) acc) (else (fold fun (fun acc (car l)) (cdr l))))))
(define identity (lambda (x) x))

;; V1 - consing on to the recursion
(define filter
  (lambda (fun l)
    (cond
      ((null? l) '())
      ((fun (car l))
       (cons (car l)
             (filter fun (cdr l))))
      (else
        (filter fun (cdr l))))))

;; (filter greater-than-two '(1 2 3 4 5))
;; => (3 4 5)

;; V2 - acc with reverse
(define acc-filter
  (lambda (fun acc l)
    (cond
      ((null? l) (reverse acc))
      ((fun (car l))
       (acc-filter fun
                   (cons (car l) acc)
                   (cdr l)))
      (else
        (acc-filter fun acc (cdr l))))))

;; (acc-filter greater-than-two '() '(1 2 3 4 5))
;; => (3 4 5)

;; V3 - filter w/ fold
(define fold-filter
  (lambda (filter-f l)
    (reverse
      (fold
        (lambda (acc e)
          (cond
            ((filter-f e)
             (cons e acc))
            (else
              acc)))
        '()
        l))))

;; (fold-filter greater-than-two '(1 2 3 4 5))
;; => (3 4 5)

;; V4 - w/ CPS
(define cps-filter
  (lambda (filter-f l acc-f)
    (cond
      ((null? l) (acc-f '()))
      (else
        (cps-filter filter-f
                    (cdr l)
                    (lambda (acc)
                      (cond
                        ((filter-f (car l))
                         (acc-f (cons (car l) acc)))
                        (else
                          (acc-f acc)))))))))

(cps-filter greater-than-two '(1 2 3 4 5) identity)
