;; Write `map` 3 different ways

; a simple mapper (aka a function you'd pass to map)
(define add-one
  (lambda (n)
    (add1 n)))

; an easy cps finalizer
(define identity
  (lambda (x) x))

;; MAP V1: consing on to the recursion
(define map
  (lambda (l fun)
    (cond
      ((null? l) '())
      (else
        (cons (fun (car l))
              (map (cdr l) fun))))))


;; MAP V2: accumulate & reverse
(define map-with-acc
  (lambda (l fun)
    (map-with-acc-helper '() l fun)))

(define map-with-acc-helper
  (lambda (acc l fun)
    (cond
      ((null? l) (reverse acc))
      (else
        (map-with-acc-helper
          (cons (fun (car l)) acc)
          (cdr l)
          fun)))))


;; MAP V3: with CPS
(define map-with-col
  (lambda (l fun col)
    (cond
      ((null? l) (col '()))
      (else
        (map-with-col (cdr l)
                      fun
                      (lambda (acc)
                        (col (cons (fun (car l)) acc))))))))


;(map '(1 2 3) add-one)
;(map-with-acc '(1 2 3) add-one)
;(map-with-col '(1 2 3) add-one identity)
