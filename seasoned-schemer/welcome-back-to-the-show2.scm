(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define atom?
  (lambda (x)
    (not (or (pair? x)
             (null? x)))))


(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (two-in-a-row-b? (car lat)
                        (cdr lat))))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? preceding (car lat))
           (two-in-a-row-b? (car lat)
                            (cdr lat)))))))

;; TEST
;(two-in-a-row? '())
;(two-in-a-row? '(a))
;(two-in-a-row? '(a c d c d d c))
;(two-in-a-row? '(a c d c x))


(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup)'())
      (else
       (cons (+ sonssf (car tup))
             (sum-of-prefixes-b (+ sonssf (car tup))
                                (cdr tup)))))))

;; TEST
;(sum-of-prefixes '())
;(sum-of-prefixes '(1))
;(sum-of-prefixes '(1 1 1 1 1))
;(sum-of-prefixes '(1 2 3 4 5))


(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n)(car lat))
      (else
       (pick (sub1 n)
             (cdr lat))))))

;; TEST
(pick 3 '(a c d c))


(define scramble
  (lambda (tup)
    (scramble-b '() tup)))

(define scramble-b
  (lambda (rev-pre tup)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b (cons (car tup) rev-pre)
                         (cdr tup)))))))


(scramble '(1 2 3 4 5 6 7 8 9 2))
