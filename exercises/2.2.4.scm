(use sicp) ; just for the picture language

(define rogers (image->painter "rogers.jpg"))

;; ex 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; ex 2.45

(define (split op1 op2)
  (define (inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (inner painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  inner)

(define right-split (split beside below))
(define up-split (split below beside))

;; ex 2.46

(use test)

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(test (make-vect 3 4) (add-vect (make-vect 1 2) (make-vect 2 2)))
(test (make-vect 1 2) (sub-vect (make-vect 3 4) (make-vect 2 2)))
(test (make-vect 3 6) (scale-vect 3 (make-vect 1 2)))

;; ex 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))

(let ((frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1))))
  (test (make-vect 0 0) (origin-frame frame))
  (test (make-vect 1 0) (edge1-frame frame))
  (test (make-vect 0 1) (edge2-frame frame)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (cdr (cdr frame)))

(let ((frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1))))
  (test (make-vect 0 0) (origin-frame frame))
  (test (make-vect 1 0) (edge1-frame frame))
  (test (make-vect 0 1) (edge2-frame frame)))

;; ex 2.48

(define (make-segment from to) (cons from to))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(let ((segment (make-segment (make-vect 1 2) (make-vect 3 4))))
  (test (make-vect 1 2) (start-segment segment))
  (test (make-vect 3 4) (end-segment segment)))

;; ex 2.49

(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define cross
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.102 0.260) (make-vect 0.244 0.410))
    (make-segment (make-vect 0.244 0.410) (make-vect 0.374 0.316))
    (make-segment (make-vect 0.374 0.316) (make-vect 0.432 0.336))
    (make-segment (make-vect 0.432 0.336) (make-vect 0.450 0.300))
    (make-segment (make-vect 0.450 0.300) (make-vect 0.374 0.170))
    (make-segment (make-vect 0.374 0.170) (make-vect 0.440 0.002))
    (make-segment (make-vect 0.548 0.002) (make-vect 0.610 0.170))
    (make-segment (make-vect 0.610 0.170) (make-vect 0.556 0.300))
    (make-segment (make-vect 0.556 0.300) (make-vect 0.574 0.336))
    (make-segment (make-vect 0.574 0.336) (make-vect 0.628 0.316))
    (make-segment (make-vect 0.628 0.316) (make-vect 0.732 0.360))
    (make-segment (make-vect 0.732 0.360) (make-vect 0.910 0.610))
    (make-segment (make-vect 0.910 0.712) (make-vect 0.732 0.489))
    (make-segment (make-vect 0.732 0.489) (make-vect 0.628 0.442))
    (make-segment (make-vect 0.628 0.442) (make-vect 0.610 0.524))
    (make-segment (make-vect 0.610 0.524) (make-vect 0.732 0.990))
    (make-segment (make-vect 0.628 0.990) (make-vect 0.548 0.712))
    (make-segment (make-vect 0.548 0.712) (make-vect 0.510 0.692))
    (make-segment (make-vect 0.510 0.692) (make-vect 0.474 0.712))
    (make-segment (make-vect 0.474 0.712) (make-vect 0.410 0.990))
    (make-segment (make-vect 0.308 0.990) (make-vect 0.410 0.524))
    (make-segment (make-vect 0.410 0.524) (make-vect 0.374 0.442))
    (make-segment (make-vect 0.374 0.442) (make-vect 0.228 0.524))
    (make-segment (make-vect 0.228 0.524) (make-vect 0.102 0.360)))))

;; ex 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define rotate180 (compose rotate90 rotate90))
(define rotate270 (compose rotate90 rotate180))

;; ex 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0)))
          (paint-top
           (transform-painter
            painter2
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;; ex 2.52

(define smile-wave
  (segments->painter
   (list
    (make-segment (make-vect 0.102 0.260) (make-vect 0.244 0.410))
    (make-segment (make-vect 0.244 0.410) (make-vect 0.374 0.316))
    (make-segment (make-vect 0.374 0.316) (make-vect 0.432 0.336))
    (make-segment (make-vect 0.432 0.336) (make-vect 0.450 0.300))
    (make-segment (make-vect 0.450 0.300) (make-vect 0.374 0.170))
    (make-segment (make-vect 0.374 0.170) (make-vect 0.440 0.002))
    (make-segment (make-vect 0.548 0.002) (make-vect 0.610 0.170))
    (make-segment (make-vect 0.610 0.170) (make-vect 0.556 0.300))
    (make-segment (make-vect 0.556 0.300) (make-vect 0.574 0.336))
    (make-segment (make-vect 0.574 0.336) (make-vect 0.628 0.316))
    (make-segment (make-vect 0.628 0.316) (make-vect 0.732 0.360))
    (make-segment (make-vect 0.732 0.360) (make-vect 0.910 0.610))
    (make-segment (make-vect 0.910 0.712) (make-vect 0.732 0.489))
    (make-segment (make-vect 0.732 0.489) (make-vect 0.628 0.442))
    (make-segment (make-vect 0.628 0.442) (make-vect 0.610 0.524))
    (make-segment (make-vect 0.610 0.524) (make-vect 0.732 0.990))
    (make-segment (make-vect 0.628 0.990) (make-vect 0.548 0.712))
    (make-segment (make-vect 0.548 0.712) (make-vect 0.510 0.692))
    (make-segment (make-vect 0.510 0.692) (make-vect 0.474 0.712))
    (make-segment (make-vect 0.474 0.712) (make-vect 0.410 0.990))
    (make-segment (make-vect 0.308 0.990) (make-vect 0.410 0.524))
    (make-segment (make-vect 0.410 0.524) (make-vect 0.374 0.442))
    (make-segment (make-vect 0.374 0.442) (make-vect 0.228 0.524))
    (make-segment (make-vect 0.228 0.524) (make-vect 0.102 0.360))

    (make-segment (make-vect 0.440 0.200) (make-vect 0.500 0.230))
    (make-segment (make-vect 0.500 0.230) (make-vect 0.560 0.200)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))
