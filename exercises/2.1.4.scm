(use test)

(define test-equal? (current-test-comparator))

;; ex 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define r1 (make-interval 0.9 1.1))
(define r2 (make-interval 1.9 2.1))

(test (+ 0.9 1.9) (lower-bound (add-interval r1 r2)))
(test (+ 1.1 2.1) (upper-bound (add-interval r1 r2)))
(test (* 0.9 1.9) (lower-bound (mul-interval r1 r2)))
(test (* 1.1 2.1) (upper-bound (mul-interval r1 r2)))
(test (/ 1.9 1.1) (lower-bound (div-interval r2 r1)))
(test (/ 2.1 0.9) (upper-bound (div-interval r2 r1)))

;; ex 2.8

;; the smallest and largest value of an interval difference can be
;; calculated by substracting the upper bound from the lower bound and
;; the lower bound from the upper bound

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(test 0.8 (lower-bound (sub-interval r2 r1)))
(test 1.2 (upper-bound (sub-interval r2 r1)))

;; ex 2.9

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

(test 0.1 (width r1))
(test 0.1 (width r2))

;; we've observed before that the width of interval addition and
;; subtraction is greater than the width of the individual intervals,
;; so it's not surprising that it's equal to the sum of the widths

(test (+ (width r1) (width r2)) (width (add-interval r1 r2)))
(test (+ (width r2) (width r1)) (width (sub-interval r2 r1)))

;; the same cannot be said about multiplication and division though as
;; demonstrated below

(test #f (test-equal? (* (width r1) (width r2)) (width (mul-interval r1 r2))))
(test #f (test-equal? (* (width r1) (width r2)) (width (div-interval r1 r2))))

;; ex 2.10

;; all intervals the tests were made with so far had positive upper
;; and lower bounds (which makes sense for resistors), but the
;; exercise hints at the case of zero being part of the interval

;; if the second interval ends at zero, you get a division by zero,
;; furthermore if it spans zero, the interval you'd divide by ends up
;; with a higher lower than upper bound

(define (reciprocal-interval x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

(define x (make-interval -0.1 0))
(define y (make-interval -0.1 0.1))
(define z (make-interval 0 0.1))

(test-error (div-interval r1 x))
(test-assert (positive? (lower-bound (reciprocal-interval y))))
(test-assert (negative? (upper-bound (reciprocal-interval y))))
(test-error (div-interval r1 z))

(define (div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y)))
    (if (or (zero? yl) (zero? yu))
        (error "Cannot divide by an interval touching zero"))
    (if (and (negative? yl) (positive? yu))
        (error "Cannot divide by an interval spanning zero")))
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(test-error (div-interval r1 x))
(test-error (div-interval r1 y))
(test-error (div-interval r1 z))

;; ex 2.11

;; mul-interval does four multiplications currently to ensure that no
;; matter what the signs of the interval boundaries are, the right
;; upper and lower bound are picked

;; possible interval signs of the bounds
;; - [- -]
;; - [- +]
;; - [+ -] (invalid, as lower > upper bound)
;; - [+ +]

;; this leaves three valid cases for an interval, if you combine each
;; case for the first interval with three cases for the second
;; interval, you get nine valid cases in total, one of which is special

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond
     ((and (negative? xl) (negative? xu) (negative? yl) (negative? yu))
      (make-interval (* xu yu) (* xl yl))) ; two multiplications
     ((and (negative? xl) (positive? xu) (negative? yl) (negative? yu))
      (make-interval (* xu yl) (* xl yl))) ; two multiplications
     ((and (positive? xl) (positive? xu) (negative? yl) (negative? yu))
      (make-interval (* xl yl) (* xu yu))) ; two multiplications
     ((and (negative? xl) (negative? xu) (negative? yl) (positive? yu))
      (make-interval (* xl yu) (* xl yl))) ; two multiplications
     ((and (negative? xl) (positive? xu) (negative? yl) (positive? yu))
      (make-interval (min (* xl yu) (* xu yl))
                     (max (* xl yl) (* xu yu)))) ; the special one!
     ((and (positive? xl) (positive? xu) (negative? yl) (positive? yu))
      (make-interval (* xu yl) (* xl yu))) ; two multiplications
     ((and (negative? xl) (negative? xu) (positive? yl) (positive? yu))
      (make-interval (* xl yu) (* xu yl))) ; two multiplications
     ((and (negative? xl) (positive? xu) (positive? yl) (positive? yu))
      (make-interval (* xl yu) (* xu yu))) ; two multiplications
     ((and (positive? xl) (positive? xu) (positive? yl) (positive? yu))
      (make-interval (* xl yl) (* xu yu)))))) ; two multiplications

;; this is the kind of code a compiler should write for me!

;; ex 2.12

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define (percent interval)
  (* (/ (width interval) (center interval)) 100))

(test 1.0 (percent (make-interval 0.99 1.01)))
(test 5.0 (percent (make-interval 0.95 1.05)))
(test 10.0 (percent (make-interval 0.9 1.1)))

(define (make-center-percent center tolerance)
  (let ((width (* center (/ tolerance 100))))
    (make-interval (- center width) (+ center width))))

(test 0.99 (lower-bound (make-center-percent 1.0 1)))
(test 0.95 (lower-bound (make-center-percent 1.0 5)))
(test 0.9 (lower-bound (make-center-percent 1.0 10)))

;; ex 2.13

;;   [x*(1-p) | x*(1+p)] * [y*(1-q) | y*(1+q)]
;; = [x*y*(1-p)*(1-q) | x*y*(1+p)*(1+q)]

;; not sure how that can be approximated, other than by [x*y | x*y]
;; (as 1-p and 1-q are equal to 1, for sufficiently small p and q)

;; ex 2.14

(print (center (div-interval (make-interval 0.9999 1.0001)
                             (make-interval 0.9999 1.0001))))
(print (percent (div-interval (make-interval 0.9999 1.0001)
                              (make-interval 0.9999 1.0001))))

(print (center (div-interval (make-interval 0.9999 1.0001)
                             (make-interval 0.9999 1.0001))))
(print (percent (div-interval (make-interval 1.9999 2.0001)
                              (make-interval 1.9999 2.0001))))

;; TODO

;; ex 2.15

;; the idea here is that any operation done on two intervals increases
;; the error of the result, therefore you'd try avoiding needless
;; computations, such as using an interval more than once in a
;; computation

;; ex 2.16

;; as argued in 2.15, depending on how often an interval is combined
;; with another, the error increases (a multiplication for instance is
;; worse than addition as it combines an interval times another interval)

;; I doubt there is a general way of solving this problem as there is
;; as numerical analysis is a special branch in mathematics dedicated
;; to finding ideal algorithms for specific interval problems
