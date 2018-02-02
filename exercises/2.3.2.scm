(use test)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;; ex 2.56

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(test 0 (deriv '(** x 0) 'x))
(test 1 (deriv '(** x 1) 'x))
(test '(* 2 x) (deriv '(** x 2) 'x))
(test '(* 3 (** x 2)) (deriv '(** x 3) 'x))
(test '(* 3 (* 2 x)) (deriv '(* 3 (** x 2)) 'x))
(test '(* 3 (** (+ x 1) 2)) (deriv '(** (+ x 1) 3) 'x))

;; ex 2.57

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(test 6 (deriv '(+ (* 1 x) (* 2 x) (* 3 x)) 'x))
(test 6 (deriv '(* 2 3 x) 'x))
(test '(+ (* x y) (* y (+ x 3))) (deriv '(* x y (+ x 3)) 'x))

;; ex 2.58a

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(test 4 (deriv '(x + (3 * (x + (y + 2)))) 'x))

;; ex 2.58b

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

(test 4 (deriv '(x + (3 * (x + (y + 2)))) 'x))
(test 4 (deriv '((3 * ((y + 2) + x)) + x) 'x))
(test 4 (deriv '(x + 3 * (x + y + 2)) 'x))
;; (test 4 (deriv '(3 * (x + y + 2) + x) 'x))

;; the last test shows that precedence isn't taken care of yet, so
;; while the above is an elegant solution, it's not enough to merely
;; change the selectors
