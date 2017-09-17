(use test (only extras random))

;; even? is already defined in R5RS

(test #t (even? 0))
(test #f (even? 1))
(test #t (even? (* (random 9001) 2))) ; 2n is always even
(test #f (even? (+ (* (random 9001) 2) 1))) ; 2n+1 is never even

(define (square x)
  (* x x)) ; I won't even bother testing this...

;; ex 1.16

;; observation:
;; 3^4 == 9^2

;; a  * b^n
;; 1    3 5 <- odd
;; 3    3 4 <- even
;; 3    9 2 <- even
;; 3   81 1 <- odd
;; 243 81 0 <- done

(define (fast-expt b n)
  (define (inner b n a)
    (cond
     ((= n 0) a)
     ((even? n) (inner (square b) (/ n 2) a))
     (else (inner b (- n 1) (* b a)))))
  (inner b n 1))

(test 243 (fast-expt 3 5))
(test 1024 (fast-expt 2 10))
(test 65536 (fast-expt 16 4))

;; ex 1.17

(define (double x)
  (* x 2))

(test 0 (double 0))
(test 2 (double 1))
(test 4 (double 2))

(define (halve x)
  (/ x 2))

(test 0 (halve 0))
(test 1 (halve 2))
(test 2 (halve 4))

(let ((x (random 9001)))
  (test x (halve (double x))))

;; 5 * 13 + 0
;; 5 * 12 + 5
;; 10 * 6 + 5
;; 20 * 3 + 5
;; 20 * 2 + 5 + 20
;; 40 * 1 + 5 + 20
;; 40 * 0 + 5 + 20 + 40
;; 5 + 20 + 40

(define (not-so-fast-* a b)
  (cond
   ((= b 0) 0)
   ((= b 1) a)
   ((even? b) (not-so-fast-* (double a) (halve b)))
   (else (+ a (not-so-fast-* a (- b 1))))))

(test 4 (not-so-fast-* 2 2))
(test 65 (not-so-fast-* 5 13))
(test 1234321 (not-so-fast-* 1111 1111))
(test 307200 (not-so-fast-* 640 480))

;; ex 1.18

(define (almost-fast-* x y)
  (define (inner x y a)
    (cond
     ((= y 0) a)
     ((even? y) (inner (double x) (halve y) a))
     (else (inner x (- y 1) (+ a x)))))
  (inner x y 0))

(test 4 (almost-fast-* 2 2))
(test 65 (almost-fast-* 5 13))
(test 1234321 (almost-fast-* 1111 1111))
(test 307200 (almost-fast-* 640 480))

;; ex 1.19

;; b : bp + aq
;; b': (bp + aq)p + (bq + aq + ap)q
;;   : bp^2 + apq + bq^2 + aq^2 + apq
;;   : aq^2 + 2apq + bp^2 + bq^2
;;   : b(p^2 + q^2) + a(2pq + q^2)

;; a : bq + aq + ap
;; a': (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;   : bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
;;   : 2aq^2 + 2apq + ap^2 + 2bpq + bq^2
;;   : b(2pq + q^2) + a(p^2 + 2pq + 2q^2)
;;   : b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2)

;; p': p^2 + q^2
;; q': q^2 + 2pq

(define (faster-fib n)
  (define (inner a b p q count)
    (cond
     ((= count 0) b)
     ((even? count)
      (inner a
             b
             (+ (square p) (square q)) ; p'
             (+ (square q) (* 2 p q)) ; q'
             (/ count 2)))
     (else (inner (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1)))))
  (inner 1 0 0 1 n))

(test 1 (faster-fib 1))
(test 1 (faster-fib 2))
(test 2 (faster-fib 3))
(test 3 (faster-fib 4))
(test 5 (faster-fib 5))
(test 8 (faster-fib 6))
(test 13 (faster-fib 7))
(test 21 (faster-fib 8))
(test 34 (faster-fib 9))
(test 55 (faster-fib 10))
