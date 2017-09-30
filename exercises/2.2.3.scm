(use test)
(define nil '())
(define true #t)
(define false #f)

;; ex 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(let ()
  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

  (test (list 1 2 3) (map (lambda (x) (+ x 1)) (list 0 1 2))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(test (list 1 2 3 4) (append (list 1 2) (list 3 4)))

(define (length sequence)
  (accumulate (lambda (_ acc) (+ acc 1)) 0 sequence))

(test 5 (length (list 1 2 3 4 5)))

;; ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(test 79 (horner-eval 2 (list 1 3 0 5 0 1)))

;; ex 2.35

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
                         (cond
                          ((null? x) 0)
                          ((not (pair? x)) 1)
                          (else (count-leaves x))))
                       tree)))

(test 4 (count-leaves (list (list 1 2) (list 3 4))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree))))

(test 4 (count-leaves (list (list 1 2) (list 3 4))))

;; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6)
                (list 7 8 9) (list 10 11 12)))
(test (list 22 26 30) (accumulate-n + 0 s))

;; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define v (list 1 2 3))
(define w (list 4 5 6))

(define m (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)))
(define n (list (list 1 0 0)
                (list 0 1 0)
                (list 0 0 1)))

(test 32 (dot-product v w))
(test 32 (dot-product w v))

(test (list (+ 1 4 9) (+ 4 10 18) (+ 7 16 27))
      (matrix-*-vector m v))
(test (list (+ 4 0 0) (+ 0 5 0) (+ 0 0 6))
      (matrix-*-vector n w))

(test (list (list 1 4 7)
            (list 2 5 8)
            (list 3 6 9))
      (transpose m))
(test (list (list 1 0 0)
            (list 0 1 0)
            (list 0 0 1))
      (transpose n))

(test (list (list 1 2 3)
            (list 4 5 6)
            (list 7 8 9))
      (matrix-*-matrix m n))
(test (list (list 30 36 42)
            (list 66 81 96)
            (list 102 126 150))
      (matrix-*-matrix m m))

;; ex 2.38

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(test (/ 1 (/ 2 (/ 3 1))) (fold-right / 1 (list 1 2 3)))
(test (/ (/ (/ 1 1) 2) 3) (fold-left / 1 (list 1 2 3)))
(test (list 1 (list 2 (list 3 nil))) (fold-right list nil (list 1 2 3)))
(test (list (list (list nil 1) 2) 3) (fold-left list nil (list 1 2 3)))

;; fold-right and fold-left will create in the same sequence if the
;; procedure is commutative

(test (+ 1 2) (+ 2 1))
(test 6 (fold-right + 0 (list 1 2 3)))
(test 6 (fold-left + 0 (list 1 2 3)))

;; ex 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(test (list 3 2 1) (reverse (list 1 2 3)))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(test (list 3 2 1) (reverse (list 1 2 3)))

;; ex 2.40

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(test (list (list 2 1) (list 3 1) (list 3 2)) (unique-pairs 3))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (smallest-divisor n)
  (define (square x)
    (* x x))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(test (list (list 2 1 3) (list 3 2 5) (list 4 1 5) (list 4 3 7)
            (list 5 2 7) (list 6 1 7) (list 6 5 11))
      (prime-sum-pairs 6))

;; ex 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(test (list (list 3 2 1) (list 4 2 1) (list 4 3 1) (list 4 3 2)
            (list 5 2 1) (list 5 3 1) (list 5 3 2) (list 5 4 1)
            (list 5 4 2) (list 5 4 3))
      (unique-triples 5))

(define (triple-sum-= triple sum)
  (= (accumulate + 0 triple) sum))

(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))

(define (sum-triples n s)
  (map make-triple-sum
       (filter (lambda (triple) (triple-sum-= triple s))
               (unique-triples n))))

(test (list (list 5 4 2 11) (list 6 3 2 11) (list 6 4 1 11))
      (sum-triples 6 11))

;; ex 2.42

;; representation: column numbers for each row
;;
;; (0 1 2 3 4 5 6 7)
;;
;;  x . . . . . . .
;;  . x . . . . . .
;;  . . x . . . . .
;;  . . . x . . . .
;;  . . . . x . . .
;;  . . . . . x . .
;;  . . . . . . x .
;;  . . . . . . . x

;; this representation ensures that no two queens can be in the same
;; row, queens can be in the same column if the column number is
;; already present in the list

;; diagonals are harder to check, consider the following board:
;;
;; (4 2 7 0 3 1 5 6)
;;
;;  . . . . x . . .
;;  . . x . . . . .
;;  . . . . . . . x
;;  x . . . . . . .
;;  . . . x . . . .
;;  . x . . . . . .
;;  . . . . . x . .
;;  . . . . . . x .

;; 2|1 is diagonal to 0|3 and 3|4 to 5|6, you can spot either by
;; walking the list and incrementing/decrementing the starting number
;; until it matches a later item

(define empty-board nil)

(define (adjoin-position new-row _k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (abs x)
  (cond
   ((< x 0) (- 0 x))
   ((= x 0) 0)
   ((> x 0) x)))

(define (safe? k positions)
  (define (safe-column? queen col positions)
    (if (null? positions)
        true
        (if (and (not (= col k))
                 (= (car positions) queen))
            false
            (safe-column? queen (+ col 1) (cdr positions)))))
  (define (safe-diagonals? queen col positions)
    (if (null? positions)
        true
        (let ((x (car positions)))
          (if (= col k)
              ;; don't compare queen with itself
              (safe-diagonals? queen (+ col 1) (cdr positions))
              (if (= (abs (- k col))
                     (abs (- queen x)))
                  false
                  (safe-diagonals? queen (+ col 1) (cdr positions)))))))
  (let ((queen (list-ref positions (- k 1))))
    (and (safe-column? queen 1 positions)
         (safe-diagonals? queen 1 positions))))

(test false (safe? 1 (list 3 3 2 4)))
(test false (safe? 2 (list 3 3 2 4)))
(test true  (safe? 3 (list 3 3 2 4)))
(test true  (safe? 4 (list 3 3 2 4)))

(test true  (safe? 1 (list 4 2 7 0 3 1 5 6)))
(test false (safe? 2 (list 4 2 7 0 3 1 5 6)))
(test true  (safe? 3 (list 4 2 7 0 3 1 5 6)))
(test false (safe? 4 (list 4 2 7 0 3 1 5 6)))
(test false (safe? 5 (list 4 2 7 0 3 1 5 6)))
(test true  (safe? 6 (list 4 2 7 0 3 1 5 6)))
(test false (safe? 7 (list 4 2 7 0 3 1 5 6)))
(test false (safe? 8 (list 4 2 7 0 3 1 5 6)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(test 0  (length (queens 3)))
(test 2  (length (queens 4)))
(test 10 (length (queens 5)))
(test 4  (length (queens 6)))
(test 40 (length (queens 7)))
(test 92 (length (queens 8)))

(define (print-solution solution)
  (let ((width (length solution)))
    (for-each
     (lambda (col)
       (let loop ((i 0))
         (if (< i width)
             (begin
               (if (= (- col 1) i)
                   (display "x")
                   (display "."))
               (display " ")
               (loop (+ i 1)))))
       (newline))
     solution)))

(print-solution (car (queens 8)))

;; ex 2.43

;; the original version of the queens procedure does an iterative
;; recursion over the result of queen-cols whereas the modification
;; does tree recursion, resulting in exponential runtime with the
;; board size as exponent
