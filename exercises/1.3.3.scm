(use test)

(current-test-epsilon 1e-04)

;; ex 1.35

;; x = 1 + 1/x
;; x^2 = x + 1
;; x^2 - x - 1 = 0 | x_1,2 = -p/2 +/- sqrt((p/2)^2 - q)
;; p = -1, q = -1, x_1,2 = 1/2 +/- sqrt(5/4) = 1/2 +/- sqrt(5)/sqrt(4) = 1/2 +/- sqrt(5)/2 = (1 +/- sqrt(5))/2

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(test 0.7390822985224023 (fixed-point cos 1.0))

(test 1.61803398874989 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.6))

;; ex 1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 4) ; 29 steps
(display "---")
(newline)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 4) ; 7 steps

;; ex 1.37

;;         n1
;; ------------------
;;           n2
;; d1 + -------------
;;              n3
;;      d2 + --------
;;
;;           d3 + ...

(define (cont-frac n d k)
  (define (frac i)
    (if (<= i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        0))
  (frac 1))

(test (/ 1 (+ 1 (/ 2 (+ 2 (/ 3 3)))))
      (cont-frac (lambda (x) x) (lambda (x) x) 3))

(test 1.6180 (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)))

(define (cont-frac n d k)
  (define (frac k acc)
    (if (= k 0)
        acc
        (frac (- k 1) (/ (n k) (+ (d k) acc)))))
  (frac k 0))

(test (/ 1 (+ 1 (/ 2 (+ 2 (/ 3 3)))))
      (cont-frac (lambda (x) x) (lambda (x) x) 3))

(test 1.6180 (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)))

;; ex 1.38

(define (e-cf n)
  (define (nominator i) 1.0)
  (define (denominator i)
    (if (= (remainder i 3) 2)
        (* (+ (quotient i 3) 1) 2)
        1))
  (+ 2 (cont-frac nominator denominator n)))

(test 2.71828 (e-cf 7))

;; ex 1.39

(define (tan-cf x k)
  (define (nominator i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (denominator i)
    (- (* 2 i) 1))
  (cont-frac nominator denominator k))

(define pi (* 4 (atan 1)))

(test (tan (/ pi 3)) (tan-cf (/ pi 3) 4))
(test (tan (/ pi 4)) (tan-cf (/ pi 4) 4))
(test (tan (/ pi 6)) (tan-cf (/ pi 6) 4))
(test (tan (/ pi 8)) (tan-cf (/ pi 8) 4))
