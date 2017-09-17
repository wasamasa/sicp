(use test (only micro-benchmark %gettime/microsecs))

;; ex 1.21

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(test 199 (smallest-divisor 199))
(test 1999 (smallest-divisor 1999))
(test 7 (smallest-divisor 19999))

;; ex 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(test #t (prime? 2))
(test #t (prime? 3))
(test #f (prime? 4))

(define runtime %gettime/microsecs)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; we don't know begin yet...
(define (search-for-primes* from to)
  (timed-prime-test from)
  (search-for-primes (+ from 2) to))

(define (search-for-primes from to)
  (if (< from to)
      (search-for-primes* from to)
      ;; print things after prime test properly
      (newline)))

(search-for-primes 1001 1020) ; ~50us
(search-for-primes 10001 10039) ; ~120ms
(search-for-primes 100001 100045) ; ~260ms
(search-for-primes 1000001 1000039) ; ~800ms

;; the results are close enough to the prediction, with greater values
;; being more accurate as the number of samples increases

;; ex 1.23

(define (better-smallest-divisor n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (better-prime? n)
  (= n (better-smallest-divisor n)))

(define (start-prime-test n start-time)
  (if (better-prime? n)
      (report-prime (- (runtime) start-time))))

(search-for-primes 1001 1020) ; ~25ms
(search-for-primes 10001 10039) ; ~58ms
(search-for-primes 100001 100045) ; ~160ms
(search-for-primes 1000001 1000039) ; ~500ms

;; the speed increase is roughly 2x which is nice I guess

;; ex 1.24

(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder
     (square (expmod base (/ exp 2) m))
     m))
   (else
    (remainder
     (* base (expmod base (- exp 1) m))
     m))))

(test 0 (expmod 2 10 2))
(test 1 (expmod 11 2 10))
(test 5 (expmod 5 3 10))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n)
    (fast-prime? n (- times 1)))
   (else #f)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1) ; not sure whether that's right...
      (report-prime (- (runtime) start-time))))

(search-for-primes 1001 1020) ; ~16ms
(search-for-primes 10001 10039) ; ~18ms
(search-for-primes 100001 100045) ; ~20ms
(search-for-primes 1000001 1000039) ; ~30ms

;; while the times do increase, the amounts are rather small which
;; smells like logarithmic complexity

;; ex 1.25

;; the difference between both approaches is that expmod calculates
;; the remainder for every intermediate step which reduces the amount
;; of exponentiation to do drastically; calculating the remainder at
;; the end skips that optimization and might even overflow into floats
;; on CHICKEN without numbers

;; ex 1.26

;; as every argument to a function call is evaluated, expmod is called
;; not once, but twice for the even case, which is repeated rather often

;; ex 1.27

(define (always-congruent? n)
  (define (inner a n)
    (cond
     ((< a n)
      (if (= (expmod a n n) (remainder a n))
          (inner (+ a 1) n)
          #f))
     ((= a n)
      #t)))
  (inner 1 n))

(test #t (always-congruent? 137)) ; actual prime
(test #t (always-congruent? 561))
(test #t (always-congruent? 1105))
(test #f (always-congruent? 1337)) ; not a prime
(test #t (always-congruent? 1729))
(test #t (always-congruent? 2465))
(test #t (always-congruent? 2821))
(test #t (always-congruent? 6601))
(test #f (always-congruent? 6666)) ; not a prime either

;; ex 1.28

(define (non-trivial-square-root x n)
  ;; too bad we don't know of let yet...
  (define rem (remainder (square x) n))
  (if (and (not (= x 1)) (not (= x (- n 1)))
           (= rem 1))
      0
      rem))

(test 1 (non-trivial-square-root 1 9)) ; edge case
(test 1 (non-trivial-square-root 8 9)) ; edge case
(test 4 (non-trivial-square-root 2 9)) ; other
(test 0 (non-trivial-square-root 3 9)) ; square root

(define (alternative-expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder
     (non-trivial-square-root (alternative-expmod base (/ exp 2) m) m)
     m))
   (else
    (remainder
     (* base (alternative-expmod base (- exp 1) m))
     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (alternative-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (best-prime? n times)
  (cond
   ((= times 0) #t)
   ((miller-rabin-test n)
    (best-prime? n (- times 1)))
   (else #f)))

;; primes
(test #t (best-prime? 23 5))
(test #t (best-prime? 137 5))
(test #t (best-prime? 373 5))
(test #t (best-prime? 773 5))
(test #t (best-prime? 911 5))

;; carmichael numbers
(test #f (best-prime? 561 5))
(test #f (best-prime? 1105 5))
(test #f (best-prime? 1729 5))
(test #f (best-prime? 2465 5))
(test #f (best-prime? 2821 5))
(test #f (best-prime? 6601 5))
