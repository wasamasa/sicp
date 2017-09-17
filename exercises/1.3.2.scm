(use test)

;; ex 1.34

(define (f g) (g 2))

(define (square x) (* x x))

(test 4 (f square))
(test 6 (f (lambda (z) (* z (+ z 1)))))

;; (f f) <- calls f with 2
;; (f 2) <- calls 2 with 2
;; (2 2) <- error, 2 is not a procedure

(test-error (f f))
