(use random-bsd cairo)

;;; SICP stuff

(define dx 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

;; original
(define (smooth-n f n)
  (repeated (smooth f) n))

;; fixed version
(define (n-smooth f n)
  ((repeated smooth n) f))

;;; graphing

(define pi (* 4 (atan 1)))

(define (fn x)
  (+ (/ (sin (* x pi 4)) 4)
     (/ (random-real) 10)
     0.3))


(define width 800)
(define height 600)

(define (setup-image ctx)
  (cairo-set-source-rgb ctx 1 1 1)
  (cairo-new-path ctx)
  (cairo-rectangle ctx 0 0 width height)
  (cairo-fill ctx)

  (cairo-set-source-rgb ctx 0 0 0)
  (cairo-set-line-width ctx 2)

  (cairo-new-path ctx)
  (cairo-move-to ctx 50 550)
  (cairo-line-to ctx 50 50)
  (cairo-stroke ctx)

  (cairo-new-path ctx)
  (cairo-move-to ctx 50 550)
  (cairo-line-to ctx 750 550)
  (cairo-stroke ctx))

(define (paint-points ctx proc steps color)
  (define (->int x) (inexact->exact (round x)))
  (apply cairo-set-source-rgb ctx color)
  (let loop ((i 0))
    (when (< i steps)
      (let* ((x (/ (+ i 1) steps))
             (y (proc x)))
        (cairo-new-path ctx)
        (cairo-rectangle ctx
                         (->int (+ (* x 700) 50))
                         (->int (+ (* (- 1 y) 500) 50))
                         2 2)
        (cairo-fill ctx))
      (loop (add1 i)))))

(define (paint-legend ctx x y text color)
  (cairo-select-font-face ctx "sans-serif"
                          CAIRO_FONT_SLANT_NORMAL
                          CAIRO_FONT_WEIGHT_NORMAL)
  (cairo-set-font-size ctx 16)
  (apply cairo-set-source-rgb ctx color)
  (cairo-move-to ctx x (+ y 16))
  (cairo-show-text ctx text))

(define (create-image path)
  (let* ((surface (cairo-image-surface-create CAIRO_FORMAT_RGB24 width height))
         (ctx (cairo-create surface)))
    (setup-image ctx)
    (paint-legend ctx 75 50 "fn" '(0 0 0))
    (paint-legend ctx 75 75 "(smooth fn)" '(1 0 0))
    (paint-legend ctx 75 100 "(smooth-n fn 2)" '(0 1 0))
    (paint-legend ctx 75 125 "(n-smooth fn 2)" '(0 0 1))
    (paint-points ctx fn 200 '(0 0 0))
    (paint-points ctx (smooth fn) 200 '(1 0 0))
    (paint-points ctx (smooth-n fn 2) 200 '(0 1 0))
    (paint-points ctx (n-smooth fn 2) 200 '(0 0 1))
    (cairo-surface-write-to-png surface path)))

(create-image "1.3.4-bonus.png")
