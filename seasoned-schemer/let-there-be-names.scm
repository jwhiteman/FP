(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define leftmost
  (lambda (los)
    (cond
      ((atom? (car los))
       (car los))
      (else
       (leftmost (car los))))))

;; TEST
;(leftmost '(a b c))
;(leftmost '((((a))) b c))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (car l))
      (else
       (cond
         ((atom? (leftmost (car l)))
          (leftmost (car l)))
         (else
          (leftmost (cdr l))))))))

;; TEST
;(leftmost '(a b c))
;(leftmost '((((a))) b c))
;(leftmost '(() b c))


(define leftmost
  (lambda (l)
    (cond
      ((null? l)'())
      ((atom? (car l))(car l))
      (else
       (let
           ((thunk (leftmost (car l))))
         (cond
           ((atom? thunk) thunk)
           (else
            (leftmost (cdr l)))))))))

;; TEST
;(leftmost '(a b c))
;(leftmost '((((a))) b c))
;(leftmost '(() b c))

;; THIS IS WRONG! SEE BELOW!
(define rember-1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (cdr l))
         (else
          (cons (car l)
                (rember-1* a (cdr l))))))
      (else
       (cons (rember-1* a (car l))
             (rember-1* a (cdr l)))))))

;; TEST
; (rember-1* 'a '((x y)(z (a) b) c (d)))

;; THIS IS WRONG! SEE BELOW!
(define rember-1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a)(cdr l))
                   (else
                    (cons (car l)(R (cdr l))))))
                (else
                 (cons (R (car l))       ;; error
                       (R (cdr l)))))))) ;; error ... these two lines will remove all occurrences of a, not just the first
      (R l))))

;; TEST
; (rember-1* 'a '((x y)(z (a) b) c (d)))
; (rember-1* 'a '((x y)(z (a) b) c (a))) ;; FAIL! the final A should not be removed!!!


(define eqlist?
  (lambda (L1 L2)
    (cond
      ((and (null? L1)
            (null? L2))
       #t)
      ((or (null? L1)
           (null? L2))
       #f)
      ((and (atom? (car L1))
            (atom? (car L2)))
       (and (eq? (car L1)(car L2))
            (eqlist? (cdr L1)(cdr L2))))
      ((or (atom? (car L1))
           (atom? (car L2)))
       #f)
      (else
       (and (eqlist? (car L1)(car L2))
            (eqlist? (cdr L1)(cdr L2)))))))

;; TEST
;(eqlist? '(a b c) '(a b c))
;(eqlist? '(a b c) '(a b x))
;(eqlist? '((x (y)) z (((q)) w) q) '((x (y)) z (((q)) w) q))
;(eqlist? '((x (y)) z (((q)) w) q) '((x (y)) z (((q)) n) q))

;; THE CORRECT VERSION
(define rember1*
  (lambda (a l)
    (cond
      ((null? l)'())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)(cdr l))
         (else (cons (car l)
                     (rember1* a (cdr l))))))
      (else
       (cond
         ((eqlist? (rember1* a (car l))
                   (car l))
          (cons (car l)
                (rember1* a (cdr l))))
         (else
          (cons (rember1* a (car l))
                (cdr l))))))))

;; TEST
; (rember1* 'a '((x y)(z (a) b) c (d)))
; (rember1* 'a '((x y)(z (a) b) c (a)))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a)(cdr l))
                   (else
                    (cons (car l)(R (cdr l))))))
                (else
                 (let
                     ((thunk (R (car l))))
                   (cond
                     ((eqlist? (car l) thunk)
                      (cons (car l) (R (cdr l))))
                     (else
                      (cons thunk (cdr l))))))))))
      (R l))))




;; TEST
; (rember1* 'a '((x y)(z (a) b) c (d)))
; (rember1* 'a '((x y)(z (a) b) c (a)))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (cond
         ((> (depth* (cdr l))
             (add1 (depth* (car l))))
          (depth* (cdr l)))
         (else
          (add1 (depth* (car l)))))))))

;; TEST
; (depth* '(a b c))
; (depth* '(a (b) c))
; (depth* '(a (b (c (d)) e) f))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let
           ((dep-car (add1 (depth* (car l))))
            (dep-cdr (depth* (cdr l))))
         (cond
           ((> dep-cdr dep-car) dep-cdr)
           (else dep-car)))))))

;; TEST
;(depth* '(a b c))
;(depth* '(a (b) c))
;(depth* '(a (b (c (d)) e) f))

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (if (> (depth* (cdr l))
              (add1 (depth* (car l))))
           (depth* (cdr l))
           (add1 (depth* (car l))))))))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (max (depth* (cdr l))
            (add1 (depth* (car l))))))))
;; TEST
; (depth* '(a b c))
; (depth* '(a (b) c))
; (depth* '(a (b (c (d)) e) f))


(define scramble
  (lambda (tup)
    (letrec
        ((S (lambda (rev-pre tup)
              (cond
                ((null? tup) '())
                (else
                 (let
                     ((extended-prefix (cons (car tup) rev-pre)))
                   (cons (P (car tup) extended-prefix)
                         (S extended-prefix (cdr tup))))))))
         (P (lambda (n l)
              (cond
                ((= 1 n)(car l))
                (else
                 (P (sub1 n)(cdr l)))))))
      (S '() tup))))

;; TEST
;(scramble '(1 2 3 4 5 6 7 8 9 10))
; (scramble '(1 1 1 2 2 2 1 1 9 2))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (car l))
      (else
       (if (atom? (leftmost (car l)))
           (leftmost (car l))
           (leftmost (cdr l)))))))


(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (car l))
      (else
       (let
           ((a (leftmost (car l)))
            (b (leftmost (cdr l))))
         (if (atom? a) a b))))))


;; THIS IS BAT-SHIT AWESOME!
(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (lm l skip)))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (out (car l)))
      (else (let ()
              (lm (car l) out)
              (lm (cdr l) out))))))
;; test
;(leftmost '())
;(leftmost '(a))
;(leftmost '(() ((()) (a)) b))


(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (letrec
           ((L (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l))
                    (skip (car l)))
                   (else
                    (let ()
                      (L (car l))
                      (L (cdr l))))))))
         (L l))))))




;; test
;(leftmost '())
;(leftmost '(a))
;(leftmost '(() ((()) (a)) b))

(define rm
  (lambda (a l oh)
    (cond
      ((null? l)(oh 'no))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (if (atom? (call/cc
                   (lambda (oh)
                     (rm a (car l) oh))))
           (cons (car l)
                 (rm a (cdr l) oh))
           (cons (rm a (car l) 0)
                 (cdr l)))))))

;; TESTS
;(call/cc
; (lambda (say)
;   (rm 'a '(x y z (() (b ()))) say)))

;(call/cc
; (lambda (say)
;   (rm 'a '(x y z (() (b (a) ()))) say)))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l c)
              (cond
                ((null? l)(c 'not-here))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (cons (car l)
                           (R (cdr l) c))))
                (else
                 (if (atom?
                      (call/cc
                       (lambda (c)
                         (R (car l) c))))
                     (cons (car l)(R (cdr l) c))
                     (cons (R (car l) '_)
                           (cdr l))))))))
      (let
          ((r* (call/cc (lambda (jump)
                          (R l jump)))))
        (if (atom? r*)
            l
            r*)))))

;; TEST
(rember1* 'z '(a b (c) (d) e f (g)))
(rember1* 'a '(x y z (() (b (a)))))