;; tl;dr fold is a fucking workhorse.

;; I wanted to write mapcat without using or writing my own concat or merge.
;; My first attempts failed, so I thought I'd try to use CPS.

;; mapcat works on lists of lists, and I soon realized that I wanted to
;; replay the continuation for each subsequent head of the list.

;; I also realized that I wanted to seed the very first run with a particular value.

;; I failed until I realized that what I wanted to do was fold.
;; I'd seed with the core-func of the continuation (identity), and keep folding the
;; the continuation-result until I got to the end of the list.

;; I need to spend time with this. Even after writing this, I'm not super comfortable with it yet.

(define identity (lambda (x) x))

(define fold
  (lambda (reducer acc l)
    (cond
      ((null? l) acc)
      (else
        (fold reducer
              (reducer acc (car l))
              (cdr l))))))

(define -map-with-cps
  (lambda (map-f l acc-f)
    (cond
      ((null? l) acc-f)    ;; returning the continuation to be fold'd instead of applying it to the empty list
      (else
        (-map-with-cps map-f
                       (cdr l)
                       (lambda (acc)
                         (acc-f (cons (map-f (car l)) acc))))))))

(define mapcat
  (lambda (map-f lol)
    ((fold
       (lambda (acc l)
         (-map-with-cps map-f l acc))
       identity
       lol)
     '())))

(mapcat add1 '((1 2 3) (4 5 6) (7 8 9)))
