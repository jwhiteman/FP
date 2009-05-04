(define atom?
  (lambda (s)
    (and (not (pair? s))
         (not (null? s)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)
            (null? l2))
       #t)
      ((or (null? l1)
           (null? l2))
       #f)
      (else
       (and (equal? (car l1)
                    (car l2))
            (eqlist? (cdr l1)
                     (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)
            (atom? s2))
       (eq? s1 s2))
      ((or (atom? s1)
           (atom? s2))
       #f)
      (else
       (eqlist? s1 s2)))))

;; tests
;; (equal? '() '())
;; (equal? '(a) '(a))
;; (equal? '(a b c) '(a b c))
;; (equal? '(a (((b (c)) d) e f) g h) '(a (((b (c)) d) e f) g h))


(define rember-all
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s)
       (rember-all s (cdr l)))
      (else
       (cons (car l)
             (rember-all s (cdr l)))))))

(rember-all '(((b (c)) d) e f) '(a (((b (c)) d) e f) g h))


(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a)
       (rember-f test? a (cdr l)))
      (else
       (cons (car l)
             (rember-f test? a (cdr l)))))))

;; tests
;; rember-f equal? '(b) '(a (b) c))
;; (rember-f = 42 '(1 2 42))
;; (rember-f eq? 'b '(a b c))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a)
         (cdr l))
        (else
         (cons (car l)
               ((rember-f test?) a (cdr l)))))))) ;; expansion vs 'typical' recursion (?)

((rember-f equal?) '(b) '(a (b) c))


(define insertL-f ;; NOT *
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else
         (cons (car l)
               ((insertL-f test?) new old (cdr l))))))))

((insertL-f equal?) '(a) '(b) '(x (b) (b) x))


(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else
         (cons (car l)
               ((insertR-f test?) new old (cdr l))))))))

((insertR-f equal?) '(a) '(b) '(x (b) (b) x))

(define left
  (lambda (new old l)
    (cons new (cons old (cdr l)))))

(define right
  (lambda (new old l)
    (cons old (cons new (cdr l)))))

(define insert-g
  (lambda (strategy test?) ;; i suppose that i could have curried this further
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (strategy new old l))
        (else
         (cons (car l)
               ((insert-g strategy test?) new old (cdr l))))))))

((insert-g left equal?) '(a) '(b) '(x (b) (b) x))
((insert-g right equal?) '(a) '(b) '(x (b) (b) x))

(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old (cdr l)))) equal?))

(insertL '(a) '(b) '(x (b) (b) x))


(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((equal? (car l) old)
       (cons new (cdr l)))
      (else
       (cons (car l)
             (subst new old (cdr l)))))))


((insert-g (lambda (new old l)
             (cons new (cdr l))) equal?) '(z) '(b) '(a (b) c))


(define operator
  (lambda (nexp)
    (car (cdr nexp))))

(define 1st-sub-expression
  (lambda (nexp)
    (car nexp)))

(define 2nd-sub-expression
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value (1st-sub-expression nexp))
          (value (2nd-sub-expression nexp))))
      (else
        (* (value (1st-sub-expression nexp))
           (value (2nd-sub-expression nexp)))))))

(value '((2 * 2) + 2))


(define atom-to-function
  (lambda (a)
    (cond
      ((eq? '+ a) +)
      (else *))))

((atom-to-function '+) 1 2 3)
((atom-to-function '*) 2 3 4)


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-expression nexp))
        (value (2nd-sub-expression nexp)))))))

(value '((2 * 2) + 2))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

(multirember 'c '(a c d c))


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

((multirember-f equal?) 'c '(a c d c))


(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-tuna
  (eq?-c 'tuna))

(define multirember-T
  (lambda (test lat)
    (cond
      ((null? lat) '())
      ((test (car lat))
       (multirember-T test (cdr lat)))
      (else
       (cons (car lat)
             (multirember-T test (cdr lat)))))))

(multirember-T eq?-tuna '(you can tune a piano but you cannot tuna fish))


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR 
                                  (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR
                                  (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR
                            (cdr lat)))))))


(multiinsertLR 'z 'a 'b '(a b c b c a))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)(col '() '()))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (l r)
                           (col (cons (car lat) l)
                                r))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (l r)
                           (col l (cons (car lat) r)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat) col)))))


(define count-left-right
  (lambda (l r)
    (cons (length l) (cons (length r) '()))))

(multiinsertLR&co 'z 'a 'b '(a b c b c a a a) count-left-right)



(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
          (evens-only* (cdr l)))))
      (else
       (cons (evens-only* (car l))
             (evens-only* (cdr l)))))))

(evens-only* '(1 2 3 (4 (5 (6)) (7)) (8) ((9)) 10))
(evens-only* '(1 2 3 4 5))

;; perhaps the most important example
;; continuations, *-functions, closures... it's got it all
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)(col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
              (lambda (output even-product odd-sum)
                 (col (cons (car l) output)
                      (* (car l) even-product)
                      odd-sum))))
         (else
          (evens-only*&co (cdr l)
              (lambda (output even-product odd-sum)
                 (col output
                      even-product
                      (+ (car l) odd-sum)))))))
      (else
       (evens-only*&co (car l)
           (lambda (no nep nos)
              (evens-only*&co (cdr l)
                  (lambda (output even-product odd-sum)
                     (col (cons no output)
                          (* nep even-product)
                          (+ nos odd-sum))))))))))

(define what-to-do-last
  (lambda (list evens odds)
    (cons evens (cons odds list))))

(evens-only*&co '(1 2 3 4 5 6 7 8 9 10) what-to-do-last)
(evens-only*&co '(1 (2 (3 (((4))))) 5 (((6))) (7) 8 (9 (10))) what-to-do-last)