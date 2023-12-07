(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (deep-reverse x)
  (define (deep-reverse-h x a)
    (if (null? x)
	a
	(deep-reverse-h
	 (cdr x)
	 (cons (let ((y (car x))) (if (pair? y) (deep-reverse y) y)) a))))
  (deep-reverse-h x ()))

(define (fringe x)
  (cond ((null? x) ())
	((pair? x) (append (fringe (car x)) (fringe (cdr x))))
	(else (list x))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch x) (car x))
(define (right-branch x) (cadr x))

(define (branch-length x) (car x))
(define (branch-structure x) (cadr x))
(define (branch-torque x) (* (branch-length x)
			     (total-weight (branch-structure x))))

(define (total-weight x)
  (cond ((null? x) 0)
	((pair? x) (+
		    (total-weight (branch-structure (left-branch x)))
		    (total-weight (branch-structure (right-branch x)))))
	(else x)))

(define (balanced? x)
  (if (not (pair? x))
      #t
      (let ((l (left-branch x))
	    (r (right-branch x)))
	(and (= (branch-torque l) (branch-torque r))
	     (balanced? (branch-structure l))
	     (balanced? (branch-structure r))))))

(define (tree-map f x)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       x))

(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))
