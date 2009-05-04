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


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat))
          #f)
         (else
          (set? (cdr lat))))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat))
       #f)
      (else
       (set? (cdr lat))))))


(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat)
                (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat)
             (makeset (cdr lat)))))))


;; '(a c d c)
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat)
             (makeset (multirember (car lat) lat)))))))


(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (cond
         ((member? (car set1) set2)
          (subset? (cdr set1) set2))
         (else
          #f))))))

;; (subset? '(a) '(a b c))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))

;; (subset? '(a) '(a b c))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))


;; (intersect '(stewed potatoes and macaroni)
;;            '(macaroni and cheese))
;;  => '(and macaroni)
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(intersect '(stewed potatoes and macaroni) '(macaroni and cheese))


;; (union '(a b c d) '(a b e)) 
;;    => '(c d a b e)

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1)
             (union (cdr set1) set2))))))

(union '(a b c d) '(a b e)) 


;; (intersectall '((a b) (b a) (a c) (d a)))
;;  => '(a)
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) ;; '((a b)) => #t
       (car l-set))        ;;          => '(a b)
      (else
       (intersect (car l-set)
                  (intersectall (cdr l-set)))))))

(intersectall '((a b) (b a) (a c) (d a)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else
       #f))))


;; (firsts '((a 1) (b 2) (c 3)))
;;  => '(a b c)
(define firsts
  (lambda (lol)
    (cond
      ((null? lol) '())
      (else
       (cons (car (car lol))
             (firsts (cdr lol)))))))

(firsts '((a 1) (b 2) (c 3)))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))


(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (build (second (car rel))
                    (first (car rel)))
             (revrel (cdr rel)))))))

(revrel '((a b) (c d) (e f) (g h)))


(define revpair
  (lambda (p)
    (build (second p)
           (first p))))


(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(revrel '((a b) (c d) (e f) (g h)))

(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))