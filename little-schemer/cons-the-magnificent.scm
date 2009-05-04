(define atom?
  (lambda (s)
    (and (not (pair? s))
         (not (null? s)))))


(define rmember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)
           (cdr lat))
          (else
           (cons (car lat)
                 (rmember a (cdr lat)))))))


(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else
           (cons (car (car l))
                 (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old (cons new (cdr lat))))
          (else
           (cons (car lat)
                 (insertR new old (cdr lat)))))))


(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new lat))
          (else
           (insertL new old (cdr lat))))))


(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst new old (cdr lat)))))))


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else
       (cons (car lat)
             (subst2 new o1 o2 (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))


(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertL new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat)
             (multisubst new old (cdr lat)))))))