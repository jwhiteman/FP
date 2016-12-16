;; fizzbuzz in lighthouse scheme
(define mod
  (lambda (n m)
    (cond
      ((< n m) n)
      (else
        (mod (- n m) m)))))

(define divisible?
  (lambda (n m)
    (zero? (mod n m))))

(define fizz?
  (lambda (n)
    (divisible? n 3)))

(define buzz?
  (lambda (n)
    (divisible? n 5)))

(define fizzbuzz?
  (lambda (n)
    (and (fizz? n)
         (buzz? n))))

(define from-to
  (lambda (n stop each-f)
    (cond
      ((<= n stop) (begin
                     (each-f n)
                     (from-to (add1 n) stop each-f)))
      (else #t))))

(define fizzbuzz
  (lambda (start stop)
    (from-to start
             stop
             (lambda (n)
               (cond
                 ((fizzbuzz? n) (display "FizzBuzz!\n"))
                 ((fizz? n) (display "Fizz\n"))
                 ((buzz? n) (display "Buzz\n"))
                 (else (begin
                         (display n)
                         (display "\n"))))))))

(fizzbuzz 1 100)
