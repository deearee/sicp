(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))

(define (acc-map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) () seq))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define (horner-eval x coeffs)
  (accumulate (lambda (cur-coeff higher) (+ cur-coeff (* higher x)))
	      0
	      coeffs))

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x) (if (pair? x)
				   (count-leaves x)
				   1))
		   t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define fold-right accumulate)

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter init seq))

(define (reverse-r seq)
  (fold-right (lambda (x y) (append y (list x))) () seq))

(define (reverse-l seq)
  (fold-left (lambda (x y) (cons y x)) () seq))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (x) (map
			(lambda (y) (list x y))
			(enumerate-interval 1 (- x 1))))
	   (enumerate-interval 1 n)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(define (k-permutations seq k)
  (if (or (null? seq) (= k 0))
      (list ())
      (flatmap (lambda (x)
		 (map (lambda (s) (cons x s))
		      (k-permutations (remove x seq) (- k 1))))
	       seq)))

(define (triple-sum n s)
  (filter (lambda (x) (= s (accumulate + 0 x)))
	  (k-permutations (enumerate-interval 1 n) 3)))

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

(define empty-board ())

(define (adjoin-position row col rest)
  (append (list (list row col)) rest))

(define (safe? k positions)
  (let ((fst (car positions))
	(rst (cdr positions)))
    (null? (filter
	    (lambda (pos)
	      (let ((x-abs (abs (- (car pos) (car fst))))
		    (y-abs (abs (- (cadr pos) (cadr fst)))))
		(or (= x-abs 0) (= x-abs y-abs))))
	    rst))))
