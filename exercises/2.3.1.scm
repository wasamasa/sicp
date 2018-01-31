(use test)

;; ex 2.53

(list 'a 'b 'c) ;=> (a b c)
(list (list 'george)) ;=> ((george))
(cdr '((x1 x2) (y1 y2))) ;=> ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;=> (y1 y2)
(pair? (car '(a short list))) ;=> false
(memq 'red '((red shoes) (blue socks))) ;=> false
(memq 'red '(red shoes blue socks)) ;=> true

;; ex 2.54

(define true #t)
(define false #f)

(let ()
  (define (equal? a b)
    (cond
     ((and (symbol? a) (symbol? b))
      (eq? a b))
     ((and (null? a) (null? b))
      true)
     ((and (pair? a) (pair? b))
      (if (eq? (car a) (car b))
          (equal? (cdr a) (cdr b))
          false))))
  (test-assert (equal? '(this is a list) '(this is a list)))
  (test-assert (not (equal? '(this is a list) '(this (is a) list)))))

;; ex 2.55

;; the quote is equivalent to wrapping (quote ...) around the
;; following s-expression, so ''abracadabra is actually (quote (quote
;; abracadabra)) which evaluated to the list (quote abracadabra)

;; the first element of that list is the symbol quote
