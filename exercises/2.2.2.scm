(use test)
(define nil '())
(define true #t)
(define false #f)

;; ex 2.24

(list 1 (list 2 (list 3 4))) ;=> (1 (2 (3 4)))
(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '()))

;; [ . | . ] -> [ . | / ]
;;   |            |
;;   1          [ . | . ] -> [ . | / ]
;;                |            |
;;                2          [ . | . ] -> [ . | / ]
;;                             |            |
;;                             3            4

;; (1 (2 (3 4)))
;;  |  \
;;  1  (2 (3 4))
;;      |  \
;;      2  (3 4)
;;          | \
;;          3  4

;; ex 2.25

(test 7 (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))))
(test 7 (car (car (list (list 7)))))
(test 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))

;; ex 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(test (list 1 2 3 4 5 6) (append x y))
(test (list (list 1 2 3) 4 5 6) (cons x y))
(test (list (list 1 2 3) (list 4 5 6)) (list x y))

;; ex 2.27

(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (if (null? items)
      nil
      (let ((item (car items)))
        (if (pair? item)
            (append (deep-reverse (cdr items)) (list (deep-reverse item)))
            (append (deep-reverse (cdr items)) (list item))))))

(define x (list (list 1 2) (list 3 4)))
(test (list (list 3 4) (list 1 2)) (reverse x))
(test (list (list 4 3) (list 2 1)) (deep-reverse x))

;; ex 2.28

(define (fringe items)
  (cond
   ((null? items) nil)
   ((pair? items) (append (fringe (car items)) (fringe (cdr items))))
   (else (list items))))

(define x (list (list 1 2) (list 3 4)))
(test (list 1 2 3 4) (fringe x))
(test (list 1 2 3 4 1 2 3 4) (fringe (list x x)))

;; ex 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a)

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define x (make-mobile (make-branch 1 2) (make-branch 2 1)))

(test 1 (branch-length (left-branch x)))
(test 2 (branch-length (right-branch x)))
(test 2 (branch-structure (left-branch x)))
(test 1 (branch-structure (right-branch x)))

;; b)

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
          (total-weight structure)
          structure)))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define y (make-mobile (make-branch 1 (make-mobile (make-branch 2 2)
                                                   (make-branch 1 3)))
                       (make-branch 2 (make-mobile (make-branch 1 2)
                                                   (make-branch 3 2)))))

(test 3 (total-weight x))
(test 9 (total-weight y))

;; c)

(define z
  (make-mobile
   (make-branch 4 (make-mobile
                   (make-branch 1 2)
                   (make-branch 2 1)))
   (make-branch 3 (make-mobile
                   (make-branch 1 (make-mobile
                                   (make-branch 1 1)
                                   (make-branch 1 1)))
                   (make-branch 1 2)))))

(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (let ((length (branch-length branch))
          (structure (branch-structure branch)))
      (if (pair? structure)
          (* length (total-weight structure))
          (* length structure))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (if (= (branch-torque left) (branch-torque right))
        (cond
         ((and (pair? (branch-structure left))
               (pair? (branch-structure right)))
          (and (mobile-balanced? (branch-structure left))
               (mobile-balanced? (branch-structure right))))
         ((pair? (branch-structure left))
          (mobile-balanced? (branch-structure left)))
         ((pair? (branch-structure right))
          (mobile-balanced? (branch-structure right)))
         (else true))
        false)))

(test true (mobile-balanced? x))
(test false (mobile-balanced? y))
(test true (mobile-balanced? z))

;; d)

;; it's sufficient to redefine right-branch and branch-structure to cdr

;; ex 2.30

(define (square-tree tree)
  (cond
   ((null? tree) nil)
   ((not (pair? tree)) (* tree tree))
   (else (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(test (list 1 (list 4 (list 9 16) 25) (list 36 49))
      (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

(define (square-tree tree)
  (map
   (lambda (sub-tree)
     (if (pair? sub-tree)
         (square-tree sub-tree)
         (* sub-tree sub-tree)))
   tree))

(test (list 1 (list 4 (list 9 16) 25) (list 36 49))
      (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

;; ex 2.31

(define (tree-map proc tree)
  (cond
   ((null? tree) nil)
   ((not (pair? tree)) (proc tree))
   (else (cons (tree-map proc (car tree))
               (tree-map proc (cdr tree))))))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

(test (list 1 (list 4 (list 9 16) 25) (list 36 49))
      (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

;; ex 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; the function builds up a call chain until it has completely
;; consumed its argument, when it's done it concatenates nil and
;; mapping over appending the last set item to the rest; the process
;; is repeated for the result of this mapping and the preceding set
;; item until all set items have been consumed, then append can
;; concatenate them

;; (subsets '(1 2 3)) therefore expands into these arguments to append:

;; - (()) and ((3))
;; - (() (3)) and ((2) (2 3))
;; - (() (3) (2) (2 3)) and ((1) (1 3) (1 2) (1 2 3))

;; result: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
