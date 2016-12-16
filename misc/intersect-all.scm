;; one of the few that gave me some problems this time around
;; the problem
;; (intersect-all '((a b c) (c a d e) (e f g h a b)))
;; => (a)

;; your second solution
(define intersect-all
  (lambda (los)
    (intersect-all-helper (cdr (cdr los))
                          (intersect (car los) (car (cdr los))))))

(define intersect-all-helper
  (lambda (los acc)
    (cond
      ((null? los) acc)
      (else
        (intersect-all-helper (cdr los) (intersect (car los) acc))))))

;; after recognizing it was a fold
(define lfold
  (lambda (op acc l)
    (cond
      ((null? l) acc)
      (else
        (lfold op (op (car l) acc) (cdr l))))))

(define intersect-all
  (lambda (los)
    (lfold intersect
           (intersect (car los) (car (cdr los)))
           (cdr (cdr los)))))

;; you didn't read or understand the directions too well, or maybe they didn't show
;; enough examples for you to see the edge cases.
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set) (car l-set)
              (else (intersect (car l-set)
                               (intersectall (cdr l-set)))))))))
