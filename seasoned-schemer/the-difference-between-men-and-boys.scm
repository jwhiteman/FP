(define x
  (cons 'chicago
        (cons 'pizza '())))

(set! x 'gone)

(set! x 'skins)


(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))


(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons x '()))))

(define omnivore
  (let ((x 'ministrone))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x '())))))


;; inner definer ?
(define omnivore2
  (lambda (food)
    (define k food)
    (cons 'apples
          (cons k '()))))

(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food
            (cons x '())))))


(define chez-nous
  (let ((temp x))
    (lambda (food)
      (set! x food)
      (set! food temp))))