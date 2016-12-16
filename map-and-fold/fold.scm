;; Write `fold` 3 different ways

;; helpers
(define identity
  (lambda (x) x))

(define cons-em!
  (lambda (acc e)
    (cons e acc)))

;; V1: "standard fold". acc style.
(define fold
  (lambda (fun acc l)
    (cond
      ((null? l) acc)
      (else
        (fold fun
              (fun acc (car l))
              (cdr l))))))

; (fold + 0 '(1 2 3))

;; notice the acc nature. the results will be reversed
;; (fold (lambda (acc e) (cons e acc))
;;      '()
;;      '(1 2 3))
;; => (3 2 1)


;; V2: summing with apply-onto-recursion.
;; notice how _not_ generic it is. not folding here.
(define sum
  (lambda (lon)
    (cond
      ((null? lon) 0)
      (else
        (+ (car lon)
           (sum (cdr lon)))))))

;; V3: a cps fold.
(define cps-fold
  (lambda (reducer-f seed l collector-f)
    (cond
      ((null? l) (collector-f seed))
      (else
        (cps-fold reducer-f
                  seed
                  (cdr l)
                  (lambda (previous)
                    (collector-f (reducer-f previous (car l)))))))))

;; (cps-fold + 0 '(1 2 3) identity)
;; => 6
;;
;; notice that CPS returns results in order
;; (cps-fold cons-em! '() '(1 2 3) identity)
;; => (1 2 3)
