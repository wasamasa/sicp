(use test)

;; ex 2.2

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define start (make-point 0 2))
(define end (make-point 4 0))
(define midpoint (midpoint-segment (make-segment start end)))
(test 2 (x-point midpoint))
(test 1 (y-point midpoint))

;; ex 2.3

;; NOTE: rectangles are assumed to be perpendicular to the coordinate
;; system's origin

;; wishful thinking: a rect has rect-width and rect-height selectors

(define (rect-perimeter rect)
  (* 2 (+ (rect-width rect) (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

;; rect implementation with two opposite points spanning the rectangle

(define (make-rect start end) (cons start end))
(define (rect-width rect) (abs (- (x-point end) (x-point start))))
(define (rect-height rect) (abs (- (y-point end) (y-point start))))

(define rect (make-rect (make-point 1 1) (make-point 3 5)))
(test 12 (rect-perimeter rect))
(test 8 (rect-area rect))

;; rect implementation with starting point, width and height

(define (make-rect start width height) (cons start (cons width height)))
(define (rect-width rect) (car (cdr rect)))
(define (rect-height rect) (cdr (cdr rect)))

(define rect (make-rect (make-point 1 1) 2 4))
(test 12 (rect-perimeter rect))
(test 8 (rect-area rect))
