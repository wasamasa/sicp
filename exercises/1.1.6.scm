;; ex 1.1

10 ;=> 10
(+ 5 3 4) ;=> 12
(- 9 1) ;=> 8
(/ 6 2) ;=> 3
(+ (* 2 4) (- 4 6)) ;=> 6
(define a 3) ;=> <undefined>
(define b (+ a 1)) ;=> <undefined>
(+ a b (* a b)) ;=> 19
(= a b) ;=> #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;=> 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;=> 16

(+ 2 (if (> b a) b a)) ;=> 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;=> 16

;; ex 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; ex 1.3

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (greatest-squares x y z)
  (cond
   ((and (<= x y) (<= x z))
    (sum-of-squares y z))
   ((and (<= y x) (<= y z))
    (sum-of-squares x z))
   ((and (<= z x) (<= z y))
    (sum-of-squares x y))))

;; ex 1.4

;; depending on whether b is greater than zero, either the plus or
;; minus procedure is chosen and applies to the remaining arguments

;; ex 1.5

;; in an normal-order interpreter, test would be expanded to (if (= x
;; 0) 0 y) and evaluated, this leads to 0 being the returned answer

;; an applicative-order interpreter would evaluate each arg before
;; application first which leads to an infinite loop as calling p
;; evaluates to calling p again
