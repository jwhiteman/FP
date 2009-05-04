(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))))


;; (two-in-a-row? '(a b c c d)
;;  => #t
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      ((null? (cdr lat)) #f) ;; too bad we can't short circuit and end here...
      (else
       (or (eq? (car lat)
                (car (cdr lat)))
           (two-in-a-row? (cdr lat)))))))

;; TESTS
;; (two-in-a-row? '(a b c c d))
;; (two-in-a-row? '(a b c d))

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (or (is-first? (car lat)
                      (cdr lat))
           (two-in-a-row? (cdr lat)))))))

;; TESTS
;; (two-in-a-row? '(a b c c d))
;; (two-in-a-row? '(a b c d))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (is-first-b? (car lat)
                    (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (two-in-a-row? lat))))))

;; TESTS
;; (two-in-a-row? '(a b c c d))
;; (two-in-a-row? '(a b c d))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (two-in-a-row-b? (car lat)
                        (cdr lat))))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) preceding)
           (two-in-a-row-b? (car lat)
                            (cdr lat)))))))

;; TESTS
;; (two-in-a-row? '(a b c c d))
;; (two-in-a-row? '(a b c d))


(define sum-of-prefixes
  (lambda (tup)
    (cond
      ((null? tup) '()) ;; not needed (see below)
      (else
       (sum-of-prefixes-b 0 tup)))))

(define sum-of-prefixes-b
  (lambda (preceding tup) ;; don't blindly name the first parameter 'preceding'...THINK about what it represents and name it accordingly (sonssf)
    (cond
      ((null? tup)'())
      (else
       (cons (+ preceding (car tup))
             (sum-of-prefixes-b (+ preceding (car tup))
                                (cdr tup)))))))
     
;; TESTS
;; (sum-of-prefixes '(1 1 1 1 1)) ;; => (1 2 3 4 5)
;; (sum-of-prefixes '())
;; (sum-of-prefixes '(1))
;; (sum-of-prefixes '(1 1))
;; (sum-of-prefixes '(1 1 1))

(define pick
  (lambda (n l)
    (cond
      ((eq? 1 n)(car l))
      (else
       (pick (sub1 n)
             (cdr l))))))

;; TESTS
; (pick 3 '(a c d c))


;; defined to take non-empty lists
(define scramble
  (lambda (l)
    (scramble-b (cons (car l) '())
                l)))

(define scramble-b
  (lambda (prefix l)
    (cond
      ((null? l)'())
      (else
       (cons '???'
             (scramble-b '???'
                         (cdr l)))))))