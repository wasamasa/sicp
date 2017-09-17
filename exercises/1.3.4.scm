(use test numbers)

(current-test-epsilon 1e-04)

;; ex 1.40

(define tolerance 0.00001)
(define dx tolerance)

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

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (square x) (* x x))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(test 3.0 (sqrt 9))

(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (y) (+ (cube y) (* a (square y)) (* b y) c)))

(test 1.0 (newtons-method (cubic 1 -1 -1) 1))

;; (x + 1)^3 = (x^2 + 2x + 1) * (x + 1)
;;           = x^3 + x^2 + 2x^2 + 2x + x + 1
;;           = x^3 + 3x^2 + 3x + 1

(test -1.0 (newtons-method (cubic 3 3 1) 1))

;; ex 1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(test 3 ((double inc) 1))

(test 21 (((double (double double)) inc) 5))

(test 261 (((double (double (double double))) inc) 5))

;; ex 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(test 49 ((compose square inc) 6))

;; ex 1.43

(define (dec x) (- x 1))

(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (dec x)))))

(test 625 ((repeated square 2) 5))

;; ex 1.44

;; NOTE: not sure how much sense a hardcoded dx makes...

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smooth-n f n)
  (repeated (smooth f) n))

;; ex 1.45

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; | nth root | dampening steps |
;; |----------+-----------------|
;; | 2-3      | 1               |
;; | 4-7      | 2               |
;; | 8-15     | 3               |

(define (log2 x)
  (/ (log x) (log 2)))

(define (dampening-steps n)
  (inexact->exact (truncate (log2 n))))

(test 1 (dampening-steps 2))
(test 1 (dampening-steps 3))
(test 2 (dampening-steps 4))
(test 2 (dampening-steps 7))
(test 3 (dampening-steps 8))
(test 3 (dampening-steps 15))
(test 4 (dampening-steps 16))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (dampening-steps n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(test 2.0 (nth-root 4 2))
(test 2.0 (nth-root 8 3))
(test 2.0 (nth-root 16 4))
(test 2.0 (nth-root 32 5))
(test 2.0 (nth-root 64 6))
(test 2.0 (nth-root 128 7))
(test 2.0 (nth-root 256 8))

;; ex 1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(test 2.0 (sqrt 4))
(test 3.0 (sqrt 9))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess))
       tolerance))
  ((iterative-improve good-enough? f) first-guess))

(test 0.7390822985224023 (fixed-point cos 1.0))
(test 1.61803398874989 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.6))
