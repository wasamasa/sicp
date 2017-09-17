(use test)

;; ex 2.4

(let () ; globally redefining cons/car/cdr breaks tests
  (define (cons x y)
    (lambda (m) (m x y)))

  (define (car z)
    (z (lambda (p q) p)))

  (define (cdr z)
    (z (lambda (p q) q)))

  (test 1 (car (cons 1 2)))
  (test 2 (cdr (cons 1 2))))

;; (cdr (cons 1 2))
;; (cdr (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) q))
;; ((lambda (p q) q) 1 2)
;; 2

;; ex 2.5

;; 2^x*3^y cannot be factorized into a different product because 2 and
;; 3 are prime factors, therefore one can calculate the exponents of 2
;; and 3 by repeatedly dividing by either of them and keeping count

(define (prime-factor-expt n k)
  (define (iter n i)
    (if (= (remainder n k) 0)
        (iter (/ n k) (+ i 1))
        i))
  (iter n 0))

(test 3 (prime-factor-expt (* (expt 2 3) (expt 3 2)) 2))
(test 2 (prime-factor-expt (* (expt 2 3) (expt 3 2)) 3))

(let () ; see above
  (define (cons x y)
    (* (expt 2 x) (expt 3 y)))

  (define (car z)
    (prime-factor-expt z 2))

  (define (cdr z)
    (prime-factor-expt z 3))

  (test 1 (car (cons 1 2)))
  (test 2 (cdr (cons 1 2))))

;; ex 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (define one (add-1 zero))
;; (define one (lambda (f) (lambda (x) (f ((zero f) x)))))
;; (define one (lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
(define one (lambda (f) (lambda (x) (f x))))

;; (define two (add-1 one))
;; (define two (lambda (f) (lambda (x) (f ((one f) x)))))
;; (define two (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) (f x))) x)))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; test time

(define inc add1) ; avoid using redefined +
(test 0 ((zero inc) 0))
(test 1 (((add-1 zero) inc) 0))
(test 2 (((add-1 (add-1 zero)) inc) 0))

(test 1 ((one inc) 0))
(test 2 ((two inc) 0))
(test 4 (((+ two two) inc) 0))
