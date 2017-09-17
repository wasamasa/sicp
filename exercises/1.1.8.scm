(use test)
(current-test-epsilon 1e-02) ; ugh

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; ex 1.6

;; The rewritten if does evaluate all of its arguments.  As the else
;; clause is another call to sqrt-iter, it will be always evaluated
;; (as opposed to only as long as the guess isn't good enough),
;; leading to a non-terminating loop.

;; ex 1.7

;; fails as 0.01 is close to 0.03
(square 0.03) ;=> 0.0009
(sqrt 0.0009) ;=> 0.0403006226465455

; (use numbers)
; (sqrt 15241578750190500) ;=> loops due to precision loss

(define (better-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? a b)
    (< (/ (abs (- b a)) a) 0.001))
  (define (sqrt-iter guess)
    (let ((new-guess (improve guess)))
      (if (good-enough? guess new-guess)
          guess
          (sqrt-iter new-guess))))
  (sqrt-iter 1.0))

;; better than old one
(better-sqrt 0.0009) ;=> 0.0300276674218256
;; worse than old one with numbers
(better-sqrt 15241578750190500) ;=> 123498039.030941

;; ex 1.8

(define (cube x)
  (* x x x))

(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* guess 2)) 3))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.00001)) ; more precision loss with cube
  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

;; tests

(test 4 (square 2))
(test 0.01 (square 0.1))

(test 2.0 (sqrt 4))
(test 0.1 (sqrt 0.01))

(test 8 (cube 2))
(test 0.001 (cube 0.1))

(test 2.0 (cbrt 8))
(test 0.1 (cbrt 0.001))
