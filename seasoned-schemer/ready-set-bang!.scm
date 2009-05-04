(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))


(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)


(define sweet-toothL
  (lambda (food)
    (set! last food)
    (sweet-tooth food)))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients 
          (cons food ingredients))
    (cons food (cons 'cake '()))))
              

(define deep
  (lambda (n)
    (cond
      ((zero? n) 'pizza)
      (else
       (cons (deep (sub1 n)) '())))))


(define Ns '())
(define Rs '())

(define deepR
  (lambda (n)
    (let ((r (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons r Rs))
      r)))


(define find
  (lambda (n Ns Rs)
    (letrec
        ((F (lambda (Ns Rs)
              (cond
                ((eq? (car Ns) n)
                 (car Rs))
                (else
                 (F (cdr Ns)(cdr Rs)))))))
      (F Ns Rs))))

(define member?
  (lambda (a lat)
    (letrec
        ((M? (lambda (l)
               (cond
                 ((null? l) #f)
                 (else
                  (or (eq? (car l) a)
                      (M? (cdr l))))))))
      (M? lat))))

(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        deepR)))

(define deep
  (lambda (n)
    (cond
      ((zero? n) 'pizza)
      (else
       (cons (deepM (sub1 n)) '())))))


;; (deepM 3)
;; => (deep 3)
;; => (cons (deepM 2) '())
;; => (cons (deep 2) '())
;; => (cons (cons (deepM 1) '()) '())
;; => (cons (cons (deep 1) '()) '())
;; => (cons (cons (cons (deepM 0) '()) '()) '())
;; => (cons (cons (cons (deep 0) '()) '()) '())
;; => (cons (cons (cons 'pizza '()) '()) '())
;; => (cons (cons '(pizza) '()) '())
;; => (cons '((pizza)) '())
;; => '(((pizza)))
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((r (deep n)))
          (set! Ns (cons n Ns))
          (set! Rs (cons r Rs))
          r))))


(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((r (deep n)))
            (set! Ns (cons n Ns))
            (set! Rs (cons r Rs))
            r)))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((F (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((eq? (car ns) n)
                 (car Rs))
                (else
                 (F (cdr ns)(cdr rs)))))))
      (F Ns Rs))))


(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((found (find n Ns Rs)))
        (if found
            found
            (let ((r (deep n)))
              (set! Ns (cons n Ns))
              (set! Rs (cons r Rs))
              r))))))



(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (add1 (length (cdr l)))))))


(define foo
  (let ((x (lambda (l) 0)))
    (set! x (lambda (arg) (x arg)))
    x))

