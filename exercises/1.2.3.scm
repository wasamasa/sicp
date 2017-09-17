;; ex 1.14

;;       (cc 11 5)
;;          |     \
;;       (cc 11 4) (cc -39 5)
;;          |     \
;;       (cc 11 3) (cc -14 4)
;;          |     `-------------------------------------.
;;       (cc 11 2)                                      (cc 1 3)
;;          |     `----------.                              \   `-----.
;;       (cc 11 1)           (cc 6 2)                        (cc 1 2) (cc -9 3)
;;      /      \            /        \                          |    \
;; (cc 11 0) (cc 10 1)  (cc 6 1)      (cc 1 2)               (cc 1 1) (cc -4 2)
;;          /   |         /     \        `--. `------.          |    \
;; (cc 10 0) (cc 9 1)  (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2)  (cc 1 0) (cc 0 1)=1
;;          /   |              /   |        |    \
;;  (cc 9 0) (cc 8 1)  (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1)=1
;;          /   |              /   |
;;  (cc 8 0) (cc 7 1)  (cc 4 0) (cc 3 1)
;;          /   |              /   |
;;  (cc 7 0) (cc 6 1)  (cc 3 0) (cc 2 1)
;;          /   |              /   |
;;  (cc 6 0) (cc 5 1)  (cc 2 0) (cc 1 1)
;;          /   |              /   |
;;  (cc 5 0) (cc 4 1)  (cc 1 0) (cc 0 1)=1
;;          /   |
;;  (cc 4 0) (cc 3 1)
;;          /   |
;;  (cc 3 0) (cc 2 1)
;;          /   |
;;  (cc 2 0) (cc 1 1)
;;          /   |
;;  (cc 1 0) (cc 0 1)=1

;; time: exponential (tree contains copies of itself that look the
;; same as the parent tree...)
;; space: linear (deepest recursion uses n 1cent pieces)

;; ex 1.15

(define (sine angle)
  (define (cube x)
    (* x x x))
  (define (p x)
    (- (* 3 x) (* 4 (cube x))))

  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; calculation of the sine argument without repeatedly applying p:
;; (sine 12.5)
;; (/ 12.5 3.0 3.0 3.0 3.0 3.0) <- division by 3.0 for x times

;; (sine 12.5)
;; (p (sine 4.16))
;; (p (p (sine 1.38)))
;; (p (p (p (sine 0.46296))))
;; (p (p (p (p (sine 0.15432)))))
;; (p (p (p (p (p (sine 0.05144))))))
;; (p (p (p (p (p 0.05144)))))
;; (p (p (p (p 0.15377))))
;; (p (p (p 0.44678)))
;; (p (p 0.98361))
;; (p -0.8557)
;; -0.06081

;; p is applied as often until the angle goes below 0.1 or in other
;; words, 12.5/3^x < 0.1 <=> 12.5 < 3^x/10 <=> 125 < 3^x <=> 4.4 < x,
;; so we must apply it five times (and have five calls to sine)

;; both space and time complexity are therefore logarithmic
