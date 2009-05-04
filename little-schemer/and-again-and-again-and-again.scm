(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))

(define Y-length?
  (Y (lambda (f)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else
          (add1 (f (cdr l)))))))))

(Y-length? '(a b c d e))


;; (looking 'caviar '(6 2 4 caviar 5 7 3))
;;  => true
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n)(car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

;; first attempt...mostly correct
; (define keep-looking
;  (lambda (a c lat)
;    (cond
;      ((eq? a c) #t)
;      (else
;       (keep-looking a (pick c lat) lat)))))


(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a)))))

(looking 'caviar '(6 2 4 caviar 5 7 3))


(define eternity
  (lambda (x)
    (eternity x)))

;; (shift '((a b) c))
;; => '(a (b c))

;; (shift '((a b) (c d)))
;; => '(a (b (c d)))
;; FAILED

(define length*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (add1 (length* (cdr l))))
      (else
       (+ (length* (car l))
          (length* (cdr l)))))))

(length* '((a b)(c d)))


(define a-pair?
  (lambda (s)
    (cond
      ((atom? s) #f)
      ((null? s) #f)
      ((null? (cdr s)) #f)
      ((null? (cdr (cdr s))) #t)
      (else
       #f))))