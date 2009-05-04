(define atom?
  (lambda (s)
    (and (not (pair? s))
         (not (null? s)))))


(define equan?
  (lambda (n m)
    (cond
      ((and (number? n)
            (number? m))
       (= n m))
      ((or (number? n)
           (number? m))
       #f)
      (else
       (eq? n m)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else
          (cons (car l)
                (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l))
             (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (insertR new old (cdr l)))))
         (else
          (insertR new old (cdr l)))))
      (else
       (cons (insertR new old (car l))
             (insertR new old (cdr l)))))))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
      (else
       (+ (occur* a (car l))
          (occur* a (cdr l)))))))

(occur* 'a '(b a (a (((c) (((a)))))) (b (a)) a))


(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l)
                (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(subst* 'house 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))


(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else
          (insertL* new old (cdr l)))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

(insertL* 'a 'c '(c d c (((c) a) c)))


(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) #t)
         (else
          (member* a (cdr l)))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))

(member* 'a '(() ((())) v ((v) v) ((((v (((((a)))))))))))
(member* 'a '(() ((())) v ((v) v) ((((v (((((z)))))))))))


(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
       (leftmost (car l))))))

(leftmost '(hello))
(leftmost '((((((((hello world) venus))))))))

(define eqlist?
  (lambda (L1 L2)
    (cond
      ((and (null? L1) (null? L2)) #t)
      ((or (null? L1) (null? L2))#f)
      ((and (atom? (car L1))
            (atom? (car L2)))
       (and (equan? (car L1) (car L2))
            (eqlist? (cdr L1) (cdr L2))))
      ((or (atom? (car L1))
           (atom? (car L2)))
       #f)
      (else
       (and (eqlist? (car L1) (car L2))
            (eqlist? (cdr L1) (cdr L2)))))))

(eqlist? '(a (b (c)) d) '(a (b (c)) d))

(define equal-i?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)
            (atom? s2))
       (equan? s1 s2))
      ((or (atom? s1)
           (atom? s2))
       #f)
      (else
       (eqlist-i? s1 s2)))))