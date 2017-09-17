(use test)

;; ex 2.1

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(test 1 (numer (make-rat 1 2)))
(test 2 (denom (make-rat 1 2)))
(test -1 (numer (make-rat -1 2)))
(test 2 (denom (make-rat -1 2)))
;; not normalized (yet)
(test 1 (numer (make-rat 1 -2)))
(test -2 (denom (make-rat 1 -2)))
(test -1 (numer (make-rat -1 -2)))
(test -2 (denom (make-rat -1 -2)))

(define (make-rat n d)
  (if (negative? d)
      (cons (- n) (- d))
      (cons n d)))

(test 1 (numer (make-rat 1 2)))
(test 2 (denom (make-rat 1 2)))
(test -1 (numer (make-rat -1 2)))
(test 2 (denom (make-rat -1 2)))
;; normalized!
(test -1 (numer (make-rat 1 -2)))
(test 2 (denom (make-rat 1 -2)))
(test 1 (numer (make-rat -1 -2)))
(test 2 (denom (make-rat -1 -2)))
