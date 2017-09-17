(use test)

;; changing 10 cents with pennies and nickles
;; 10x1c
;; 2x5c
;; 1x5c, 5x1c

;; http://stackoverflow.com/a/39648617

;;                         (cc 10 2)
;;                            |
;;          +-----------------+--------------+
;;          |                                |
;;       (cc 10 1)                        (cc 5 2)
;;          |                                |
;;    +-----+-----+                     +----+----+
;;    |           |                     |         |
;; (cc 10 0)=0 (cc 9 1)              (cc 5 1)  (cc 0 2)=1
;;                |                     |
;;    +-----------+               +-----+-----+
;;    |           |               |           |
;; (cc 9 0)=0  (cc 8 1)        (cc 5 0)=0  (cc 4 1)
;;                |                           |
;;    +-----------+               +-----------+
;;    |           |               |           |
;; (cc 8 0)=0  (cc 7 1)        (cc 4 0)=0  (cc 3 1)
;;                |                           |
;;    +-----------+               +-----------+
;;    |           |               |           |
;; (cc 7 0)=0  (cc 6 1)        (cc 3 0)=0  (cc 2 1)
;;                |                           |
;;    +-----------+               +-----------+
;;    |           |               |           |
;; (cc 6 0)=0  (cc 5 1)        (cc 2 0)=0  (cc 1 1)
;;                |                           |
;;    +-----------+               +-----------+
;;    |           |               |           |
;; (cc 5 0)=0  (cc 4 1)        (cc 1 0)=0  (cc 0 1)=1
;;                |
;;    +-----------+
;;    |           |
;; (cc 4 0)=0  (cc 3 1)
;;                |
;;    +-----------+
;;    |           |
;; (cc 3 0)=0  (cc 2 1)
;;                |
;;    +-----------+
;;    |           |
;; (cc 2 0)=0  (cc 1 1)
;;                |
;;    +-----------+
;;    |           |
;; (cc 1 0)=0  (cc 0 1)=1


;; ex 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(test 0 (f 0))
(test 1 (f 1))
(test 2 (f 2))
(test 4 (f 3))
(test 11 (f 4))
(test 25 (f 5))
(test 59 (f 6))

;; 1*1, 1*1 -> 2
;; 1*1, 1*2 -> 3
;; 1*2, 1*3 -> 5
;; 1*3, 1*5 -> 8
;; 1*5, 1*8 -> 13

;; 0, 1, 2
;; 3*0, 2*1, 1*2 -> 4
;; 3*1, 2*2, 1*4 -> 11
;; 3*2, 2*4, 1*11 -> 25
;; 3*4, 2*11, 1*25 -> 59
;; ...

(define (f n)
  (define (inner a b c n)
    (if (= n 0)
        c
        (inner (+ (* 3 c) (* 2 b) a) a b (- n 1))))
  (inner 2 1 0 n))

(test 0 (f 0))
(test 1 (f 1))
(test 2 (f 2))
(test 4 (f 3))
(test 11 (f 4))
(test 25 (f 5))
(test 59 (f 6))

;; ex 1.12

(define (pascal a b)
  (cond
   ((= b 0) 1)
   ((= a b) 1)
   (else (+ (pascal (- a 1) (- b 1))
            (pascal (- a 1) b)))))

(test 1 (pascal 0 0))

(test 1 (pascal 1 0))
(test 1 (pascal 1 1))

(test 1 (pascal 2 0))
(test 2 (pascal 2 1))
(test 1 (pascal 2 2))

(test 1 (pascal 3 0))
(test 3 (pascal 3 1))
(test 3 (pascal 3 2))
(test 1 (pascal 3 3))

(test 1 (pascal 4 0))
(test 4 (pascal 4 1))
(test 6 (pascal 4 2))
(test 4 (pascal 4 3))
(test 1 (pascal 4 4))

(define (pascal-triangle n)
  (let row ((x 0))
    (if (< x n)
        (begin
          (display (make-string (- n x 1) #\space))
          (let col ((y 0))
            (if (<= y x)
                (begin
                  (display (pascal x y))
                  (display " ")
                  (col (+ y 1)))))
          (newline)
          (row (+ x 1))))))

(pascal-triangle 5)
