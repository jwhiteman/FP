(define atom?
  (lambda (x)
    (not (or (pair? x)
             (null? x)))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

;; TEST
; (multirember 'tuna '(shrimp salad tuna salad and tuna))


(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (mr (cdr lat)))
                  (else
                   (cons (car lat)
                         (mr (cdr lat))))))))
       mr)
     lat)))

;; TEST
; (multirember 'tuna '(shrimp salad tuna salad and tuna))


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat)'())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

;; TEST
; ((multirember-f eq?) 'c '(a c d c))


(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f (lambda (a lat)
                (cond
                  ((null? lat) '())
                  ((test? (car lat) a)
                   (m-f a (cdr lat)))
                  (else
                   (cons (car lat)
                         (m-f a (cdr lat))))))))
      m-f)))

;; TEST
;; ((multirember-f eq?) 'c '(a c d c))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (member a (cdr lat)))))))

;; TEST
; (member? 'd '(a c d c))

(define member?
  (lambda (a lat)
    (letrec
        ((M (lambda (lat)
              (cond
                ((null? lat) #f)
                (else
                 (or (eq? (car lat) a)
                     (M (cdr lat))))))))
      (M lat))))

;; TEST
;(member? 'd '(a c d c))
;(member? 'x '(a c d c))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))

;; TEST
;; (union '(tomatoes and macaroni casserole) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set1)
              (cond
                ((null? set1) set2)
                ((member? (car set1) set2)
                 (U (cdr set1)))
                (else
                 (cons (car set1)
                       (U (cdr set1))))))))
      (U set1))))

;; TEST
; (union '(tomatoes and macaroni casserole) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set1)
              (cond
                ((null? set1) set2)
                ((M? (car set1) set2)
                 (U (cdr set1)))
                (else
                 (cons (car set1)
                       (U (cdr set1)))))))
         (M? (lambda (a lat)
                    (cond
                      ((null? lat) #f)
                      (else
                       (or (eq? (car lat) a)
                           (M? a (cdr lat))))))))
      (U set1))))

;; TEST
; (union '(tomatoes and macaroni casserole) '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set1)
              (cond
                ((null? set1) set2)
                ((M? (car set1) set2)
                 (U (cdr set1)))
                (else
                 (cons (car set1)
                       (U (cdr set1)))))))
         (M? (lambda (a lat)
               (letrec
                   ((m? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            (else
                             (or (eq? (car lat) a)
                                 (m? (cdr lat)))))))) (m? lat)))))
      (U set1))))

;; TEST
; (union '(tomatoes and macaroni casserole) '(macaroni and cheese))


(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (letrec
           ((T (lambda (pre lis)
                 (cond
                   ((null? lis) #f)
                   (else
                    (or (eq? (car lis) pre)
                        (T (car lis)
                           (cdr lis))))))))
         (T (car lat)
            (cdr lat)))))))


(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((W (lambda (pre lat)
              (cond
                ((null? lat) #f)
                (else
                 (or (eq? (car lat) pre)
                     (W (car lat)
                        (cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else
         (W (car lat)
            (cdr lat)))))))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else
               (or (eq? (car lat) a)
                   (W (car lat)
                      (cdr lat))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else
         (W (car lat)
            (cdr lat)))))))

;; TEST
;; (two-in-a-row? '())
;; (two-in-a-row? '(a))
;; (two-in-a-row? '(a b c d e))
;; (two-in-a-row? '(a b c d d e))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sonssf tup)
              (cond
                ((null? tup) '())
                (else
                 (cons (+ sonssf (car tup))
                       (S (+ sonssf (car tup))
                          (cdr tup))))))))
      (S 0 tup))))

;; TEST
; (sum-of-prefixes '())
; (sum-of-prefixes '(1))
; (sum-of-prefixes '(1 1 1 1 1))
; (sum-of-prefixes '(1 2 3 4 5))

(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (n lat)
              (cond
                ((= 1 n)(car lat))
                (else
                 (P (sub1 n)
                    (cdr lat))))))
         (S (lambda (pre tup)
              (cond
                ((null? tup) '())
                (else
                 (cons (P (car tup)
                             (cons (car tup) pre))
                       (S (cons (car tup) pre)
                          (cdr tup))))))))
      (S '() tup))))

;; TEST
; (scramble '(1 2 3 4 5 6 7 8 9 10))
; (scramble '(1 1 1 1 1 1 1 1 9 2))