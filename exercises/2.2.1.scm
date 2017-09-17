(use test)

(define nil '())
(define true #t)

;; ex 2.17

(define (last-pair items)
  (define (inner items)
    (let ((tail (cdr items)))
      (if (null? tail)
          items
          (inner tail))))
  (if (null? items)
      (error "last-pair is not defined for the empty list"))
  (inner items))

(test-error (last-pair nil))
(test (list 34) (last-pair (list 23 72 149 34)))

;; ex 2.18

(define (reverse items)
  (define (inner items acc)
    (if (null? items)
        acc
        (inner (cdr items) (cons (car items) acc))))
  (inner items nil))

(test nil (reverse nil))
(test (list 25 16 9 4 1) (reverse (list 1 4 9 16 25)))

;; ex 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(test 292 (cc 100 us-coins))

;; the order doesn't seem to change the result which doesn't really
;; surprise me, as long as you use all possible kinds of coins, it
;; shouldn't matter which order you did them in

;; ex 2.20

(define (filter pred items)
  (define (inner items acc)
    (if (null? items)
        (reverse acc)
        (let ((item (car items)))
          (if (pred item)
              (inner (cdr items) (cons item acc))
              (inner (cdr items) acc)))))
  (inner items nil))

(define (same-parity x . items)
  (if (even? x)
      (cons x (filter even? items))
      (cons x (filter odd? items))))

(test (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7))
(test (list 2 4 6) (same-parity 2 3 4 5 6 7))

;; ex 2.21

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(test (list 10 2.5 11.6 17) (map abs (list -10 2.5 -11.6 17)))
(test (list 1 4 9 16) (map (lambda (x) (* x x)) (list 1 2 3 4)))

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(test (list 1 4 9 16) (square-list (list 1 2 3 4)))

(define (square-list items)
  (map square items))

(test (list 1 4 9 16) (square-list (list 1 2 3 4)))

;; ex 2.22

;; if you cdr down a list (x y z) and cons up a new one from its
;; elements, the intermediate results will be (x), (y x) and (z y x)
;; because cons prepends the new element

;; doing it the other way around will not correct this, instead it
;; will create a pair of null and x, then a pair of that and y, then a
;; pair of that and z

;; ex 2.23

(define (for-each proc items)
  (if (null? items)
      true
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each (lambda (x)
            (display x)
            (newline))
          (list 57 321 88))
