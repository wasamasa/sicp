(use test)
(current-test-epsilon 1e-02) ;_;

;; ex 1.29

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(test 0.25 (integral cube 0 1 0.001))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (multiplicand x)
    (cond
     ((= x 0) 1)
     ((odd? x) 4)
     ((even? x) 2)
     ((= x n) 1)))
  (define (iter i) (* (multiplicand i) (y i)))
  (define (next x) (+ x 1))
  (* (sum iter 1 next n) (/ h 3)))

(test 0.25 (simpson-integral cube 0 1 1000))

;; ex 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n) (+ n 1))

(test 55 (sum identity 1 inc 10))
(test 3025 (sum cube 1 inc 10))

;; ex 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(test 1 (factorial 0))
(test 1 (factorial 1))
(test 2 (factorial 2))
(test 6 (factorial 3))
(test 24 (factorial 4))

(define (dec n) (- n 1))

(define (pi-product n)
  (define (nominator i) (if (odd? i) (+ i 1) (+ i 2)))
  (define (denominator i) (if (odd? i) (+ i 2) (+ i 1)))
  (define (iter i) (/ (nominator i) (denominator i)))
  (* 4 (product iter 1 inc n)))

(test 3.141 (pi-product 100))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(test 24 (factorial 4))
(test 3.141 (pi-product 100))

;; ex 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(test 55 (sum identity 1 inc 10))
(test 24 (factorial 4))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(test 55 (sum identity 1 inc 10))
(test 24 (factorial 4))

;; ex 1.33

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(test 25 (filtered-accumulate odd? + 0 identity 0 inc 10))
(test 30 (filtered-accumulate even? + 0 identity 0 inc 10))

(define (square x) (* x x))

(define (prime? x)
  (define (smallest-divisor n)
    (define (next x)
      (if (= x 2) 3 (+ x 2)))
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond
       ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= x (smallest-divisor x)))

(test #t (prime? 199))
(test #t (prime? 1999))
(test #f (prime? 19999))

(define (squared-primes-sum a b)
  (filtered-accumulate prime? + 0 square a inc b))

(test 87 (squared-primes-sum 2 10))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? x y)
  (= (gcd x y) 1))

(test #t (relatively-prime? 4 9))
(test #t (relatively-prime? 14 15))
(test #f (relatively-prime? 14 21)) ; shared prime: 7

(define (relative-primes-product n)
  (define (relative-prime x) (relatively-prime? x n))
  (filtered-accumulate relative-prime * 1 identity 0 inc n))

(test 189 (relative-primes-product 10))
