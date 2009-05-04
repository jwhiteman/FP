(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define-syntax letcc
  (syntax-rules ()
    ((letcc <continuation> <body> ...)
     (call-with-current-continuation
      (lambda (<continuation>) <body> ...)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))))


(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1)
                       set2)))
      (else
       (intersect (cdr set1)
                  set2)))))

; (intersect '(a b c) '(b c))

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else
                 (I (cdr set)))))))
      (cond
        ((null? set2) '())
        (else (I set1))))))


(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset))(car lset))
      (else
       (intersect (car lset)
                  (intersectall (cdr lset)))))))

; (intersectall '((a b)(c b)(d b)))

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop '()))
                  ((null? (cdr lset))
                   (car lset))
                  (else
                   (intersect (car lset)
                              (A (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else
           (A lset)))))))


(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop '(sorry)))
                  ((null? (cdr lset))
                   (car lset))
                  (else
                   (I (car lset)
                      (A (cdr lset)))))))
           (I (lambda (set1 set2)
                (letrec
                    ((I-b (lambda (set)
                            (cond
                              ((null? set) '())
                              ((M? (car set) set2)
                               (cons (car set)
                                     (I-b (cdr set))))
                              (else
                               (I-b (cdr set)))))))
                  (cond
                    ((null? set2)(hop '(so so sorry)))
                    (else
                     (I-b set1))))))
           (M? (lambda (a lat)
                 (letrec
                     ((M-b? (lambda (l)
                              (cond
                                ((null? l) #f)
                                (else
                                 (or (eq? (car l) a)
                                     (M-b? (cdr l))))))))
                   (M-b? lat)))))
        (cond
          ((null? lset) '())
          (else
           (A lset)))))))

;; TEST
;(intersectall '())
;(intersectall '(() (b)))
;(intersectall '((b) ()))
;(intersectall '((b)))
;(intersectall '((a) (b)))
;(intersectall '((b) (b)))
;(intersectall '((a b c)(d e f)(g h i)(j k l)))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (cdr lat))
      (else
       (cons (car lat)
             (rember a (cdr lat)))))))

; TEST
; (rember 'a '())
; (rember 'a '(a))
; (rember 'a '(d c b a))
; (rember 'a '(x y z))

(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((eq? (car l) a)
                 (cdr l))
                (else
                 (cons (car l)
                       (R (cdr l))))))))
      (R lat))))

; TEST
; (rember 'a '())
; (rember 'a '(a))
; (rember 'a '(d c b a))
; (rember 'a '(x y z))

(define rember-beyond-first
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) '())
      (else
       (cons (car lat)
             (rember-beyond-first a (cdr lat)))))))

; TEST
; (rember-beyond-first 'd '(a b c d e f g))


(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a)
                 '())
                (else
                 (cons (car lat)
                       (R (cdr lat))))))))
      (R lat))))

; TEST
; (rember-beyond-first 'x '(nothing removed!))
; (rember-beyond-first 'd '(a b c d e f g))


(define rember-upto-last
  (lambda (a lat)
    (letcc hop
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (cond
                     ((member? a (cdr lat))
                      (R (cdr lat)))
                     (else
                      (hop (cdr lat)))))
                  (else
                   (cons (car lat)
                         (R (cdr lat))))))))
        (R lat)))))

;; TEST
; (rember-upto-last 'a '(x y z))
; (rember-upto-last 'c '(a b c d e))
; (rember-upto-last 'c '(a c d c e))


;; book version
(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a)
                   (skip (R (cdr lat)))) ; cleverness
                  (else
                   (cons (car lat)
                         (R (cdr lat))))))))
        (R lat)))))

;; TEST
(rember-upto-last 'a '(x y z))
(rember-upto-last 'c '(a b c d e))
(rember-upto-last 'c '(a c d c e))