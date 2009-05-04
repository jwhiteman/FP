(define pascals-triangle
  (lambda (n)
    (reverse (generate-data n))))


;;; this is wicked inefficient, i know, but i'm still learning
(define generate-data
  (lambda (n)
    (cond
      ((= 1 n) '(1))
      ((= 2 n) '((1 1) (1)))
      (else
       (cons (build-this-row (previous-row (generate-data (sub1 n)))) ;; this application of generate-data should be saved so that it can be resused below
             (generate-data (sub1 n)))))))


(define previous-row
  (lambda (l)
    (car l)))

(define build-this-row
  (lambda (l)
    (cond
      ((start-of-row? l)
       (cons 1 (cons (next-cell l)
                     (build-this-row (cdr l)))))
      ((= 1 (car l)) '(1))
      (else
       (cons (next-cell l)
             (build-this-row (cdr l)))))))

(define start-of-row?
  (lambda (l)
    (and (= 1 (car l))
         (not (null? (cdr l))))))

(define next-cell
  (lambda (l)
    (+ (first l)
       (second l))))

;;; *****************
(pascals-triangle 20)
;;; *****************
