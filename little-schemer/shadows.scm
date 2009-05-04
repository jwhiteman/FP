(define atom?
  (lambda (n)
    (and (not (pair? n))
         (not (null? n)))))


(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))


(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp)
      ((eq? (car (cdr aexp)) 'X) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp))
                                      (numbered? (car (cdr (cdr aexp))))))
      (else
       #f)))))

(numbered? '((1 + 2) X (2 ^ 3)))
  
  
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else
       (^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))


(define value
  (lambda (nexp)
    ((atom? nexp) nexp)
    ((eq? (car nexp) '+)
     (+ (value (cdr nexp))
        (value (cdr (cdr nexp)))))
    ((eq? (car nexp) '*)
     (* (value (cdr nexp))
        (value (cdr (cdr nexp)))))
    (else
     (^ (value (cdr nexp))
        (value (cdr (cdr nexp)))))))

;;; '(+ 1 2)
(define 1st-sub-expression
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-expression
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value (1st-sub-expression nexp))
          (value (2nd-sub-expression nexp))))
      ((eq? (operator nexp) '*)
       (* (value (1st-sub-expression nexp))
          (value (2nd-sub-expression nexp))))
      (else
       (^ (value (1st-sub-expression nexp))
          (value (2nd-sub-expression nexp)))))))

(value '(+ (* 2 3) (* 4 5)))

(define sero?
  (lambda (n)
    (null? n)))

(sero? '())

;; '()
;; '( () )
;; '( () () )
(define edd1
  (lambda (n)
    (cons '() n)))

(edd1 (edd1 (edd1 '())))

(define zub1
  (lambda (n)
    (cdr n)))


(define shadow-plus
  (lambda (n m)
    (cond
      ((sero? n) m)
      (else
       (edd1 (shadow-plus (zub1 n) m))))))

(shadow-plus '( () () ()) '( () () ))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else
       (and (atom? (car l))
            (lat? (cdr l)))))))

(lat? '())
(lat? '(a b c d))
(lat? '(a b (c) d))