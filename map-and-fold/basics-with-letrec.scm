;; letrec practice
;; fold
;; map
;; filter
;; mapcat

(define (fold fold-f acc l)
  (letrec
    ((FOLD (lambda (acc l)
             (cond
               ((null? l) acc)
               (else
                 (FOLD (fold-f acc (car l))
                       (cdr l)))))))
    (FOLD acc l)))

(define map
  (lambda (map-f l)
    (letrec
      ((MAP (lambda (l)
              (cond
                ((null? l) '())
                (else
                  (cons (map-f (car l))
                        (MAP (cdr l))))))))
      (MAP l))))


(define filter
  (lambda (filter-f l)
    (letrec
      ((FILTER (lambda (l)
                 (cond
                   ((null? l) '())
                   ((filter-f (car l))
                    (cons (car l)
                          (FILTER (cdr l))))
                   (else
                     (FILTER (cdr l)))))))
      (FILTER l))))


;; not a fan of this style. it's hard to reason about.
(define mapcat
  (lambda (map-f l)
    (letrec
      ((CONCAT (lambda (list-1 list-2)
                 (cond
                   ((null? list-1) list-2)
                   (else
                     (cons (car list-1)
                           (CONCAT (cdr list-1) list-2))))))
       (FOLD (lambda (fold-f acc l)
               (cond
                 ((null? l) acc)
                 (else
                   (FOLD fold-f
                         (fold-f acc (car l))
                         (cdr l))))))
       (MAP (lambda (l)
              (cond
                ((null? l) '())
                (else
                  (cons (map-f (car l))
                        (MAP (cdr l)))))))
       (MAPCAT (lambda (l)
                 (FOLD (lambda (acc e)
                         (CONCAT acc (MAP e)))
                       '()
                       l))))
      (MAPCAT l))))

(mapcat add1 '((1 2 3) (4 5 6)))
