(define atom?
  (lambda (n)
    (and (not (pair? n))
         (not (null? n)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))))

;(member? 'a '(a c d c))
;(member? 'a '(c d))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1)
                   set2)))))

; (intersect '(a b c) '(x y a c))

;; (intersectall '( (a b) (c b) (d b) (e b) ))
;; => '(b)
(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset))(car lset))
      (else
       (intersect (car lset)
                  (intersectall (cdr lset)))))))

;(intersectall '((a b)(c d)(e f)(g h)))
; (intersectall '( (a b) (c b) (d b) (e b) ))


(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (skip)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (skip '()))
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
                     ((null? set2)(skip '()))
                     (else (I-b set1))))))
            (M? (lambda (a lat)
                 (letrec
                     ((M-b? (lambda (lat)
                              (cond
                                ((null? lat) #f)
                                (else
                                 (or (eq? (car lat) a)
                                     (M-b? (cdr lat))))))))
                   (M-b? lat)))))
         (A lset))))))
    
;; TEST
; (intersectall '((a b)(c d)(e f)(g h)))
; (intersectall '( (a b) (c b) (d b) (e b) ))


;; (rember-upto-last 'c '(a c d c e f))
;; => '(e f)
;; (rember-upto-last 'c '(x y z))
;; => '(x y z)
(define rember-upto-last
  (lambda (a lat)
    (call/cc
     (lambda (jump!)
       (letrec
           ((R (lambda (lat)
                 (cond
                   ((null? lat) '())
                   ((eq? (car lat) a)
                    (jump! (R (cdr lat))))
                   (else
                    (cons (car lat)
                          (R (cdr lat))))))))
         (R lat))))))


;; TEST
(rember-upto-last 'c '(x y z))
(rember-upto-last 'c '(a b c d e))
(rember-upto-last 'c '(a c d c e f))
(rember-upto-last 'c '())
(rember-upto-last 'c '(a b c d c))