;; this is more map than fold. look at the structure.
;; I am repeating `(fun acc (car l))`, 
(define mapfold
  (lambda (acc l fun)
    (cond
      ((null? l) '())
      (else
        (cons (fun acc (car l))
              (mapfold (fun acc (car l))
                       (cdr l)
                       fun))))))

(define sum-of-prefixes
  (lambda (lon)
    (mapfold 0 lon (lambda (acc n) (+ acc n)))))

; (sum-of-prefixes '(1 1 1 1 1))


;; trying more from the fold side of things...
(define fold
  (lambda (fold-f acc l)
    (cond
      ((null? l) acc)
      (else
        (fold fold-f
              (fold-f acc (car l))
              (cdr l))))))

;; I don't really have a mapfold like above, because it's just calling fold.
;; The onus is on the reducer, in this case.
;; I don't like that the reducer has to ask (null? acc) each time.
(define sum-of-prefixes
  (lambda (tup)
    (reverse
      (fold (lambda (acc n)
              (cond
                ((null? acc) (cons n '()))
                (else
                  (cons (+ n (car acc)) acc))))
            '()
            tup))))

(sum-of-prefixes '(1 2 3 4 5))
