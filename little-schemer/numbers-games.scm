
(define atom?
  (lambda (s)
    (and (not (pair? s))
         (not (null? s)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else
       (and (atom? (car l))
           (lat? (cdr l)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))


(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))


(define o+
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else
       (add1 (o+ x (sub1 y)))))))

(define o-
  (lambda (x y)
    (cond
      ((zero? x) y)
      (else
       (sub1 (o- (sub1 x) y))))))

(define addtup
  (lambda (lon)
    (cond
      ((null? lon) 0)
      (else
       (+ (car lon)
          (addtup (cdr lon)))))))


(define one?
  (lambda (n)
    (eq? 0 (sub1 n))))

(define X
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
       (+ n (X n (sub1 m)))))))


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1)
            (null? tup2))
       '())
      (else
       (cons (+ (car tup1)
                (car tup2))
             (tup+ (cdr tup1)
                   (cdr tup2)))))))

(define tup+-i
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1)
                (car tup2))
             (tup+-i (cdr tup1)
                     (cdr tup2)))))))

(tup+-i '(1 2 3) '(4 5 6))
(tup+-i '(1 2) '(4 5 6))
(tup+-i '(1 2 3) '(4 5))

;;; you fucked this up. [FIXED]
(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (gt (sub1 n)
           (sub1 m))))))

(gt 2 1)
(gt 1 2)
(gt 1 1)

(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (lt (sub1 n)
           (sub1 m))))))

(lt 1 2)
(lt 2 1)
(lt 1 1)

(define eqq?
  (lambda (n m)
    (not (or (< n m)
             (> n m)))))

(eqq? 5 5)
(eqq? 4 5)
(eqq? 5 4)


(define expp
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
       (X n (expp n (sub1 m)))))))

(expp 1 1)
(expp 2 3)
(expp 5 3)

(define length-i
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length-i (cdr lat)))))))
(length-i '(a b c))


(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons (car lat)
             (rempick (sub1 n)
                      (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat))
          (no-nums (cdr lat)))
         (else
          (cons (car lat)
                (no-nums (cdr lat)))))))))

(no-nums '(a 1 b 2 c 3 4 e))


(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((number? (car lat))
          (cons (car lat)
                (all-nums (cdr lat))))
         (else
          (all-nums (cdr lat))))))))

(all-nums '(a 1 b 2 c 3 4 e))

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

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat) a)
          (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))

(display (all-nums '(a 1 b 2 c 3 4 e)))