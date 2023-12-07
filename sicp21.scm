;; Rats

(define (add-rat x y)
  (make-rat (+ (* (denom y) (numer x)) (* (denom x) (numer y)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (add-rat x (mul-rat y (make-rat -1 1))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (mul-rat x (make-rat (denom y) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d))
	(s (if (< d 0) -1 1)))
    (cons (/ n g s) (/ d g s))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat 1 3))
(define two-thirds (make-rat 2 3))

(print-rat (make-rat -1 -1))
(print-rat (sub-rat one-third two-thirds))

;; Segs

(define (start-seg x) (car x))
(define (end-seg x) (cdr x))
(define (make-seg p r) (cons p r))

(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-point x y)
  (cons x y))

(define (midpoint-seg x)
  (let ((start (start-seg x))
	(end (end-seg x)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
		(/ (+ (y-point start) (y-point end)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-seg (make-seg (make-point 0 0)
				     (make-point 1 1))))

;; Cons

(define (cons1 a b)
  (lambda (x)
    (cond ((= x 0) a)
	  ((= x 1) b)
	  (else (error "Invalid cons1 parameter" x)))))
(define (car1 x) (x 0))
(define (cdr1 x) (x 1))

(define (cons2 a b)
  (lambda (m) (m a b)))
(define (car2 x) (x (lambda (x y) x)))
(define (cdr2 x) (x (lambda (x y) y)))

(define mod remainder)

(define (cons-n a b)
  (cond ((= b -1) 1)
	((= a -1)
	 (* 3 (cons-n a (- b 1))))
	(else (* 2 (cons-n (- a 1) b)))))

(define (car-n x)
  (if (= (mod x 4) 0)
      (+ 1 (car-n (/ x 2)))
      0))

;; Nats

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-church a b)
  ((a add-1) b))

(((add-church (add-church two two) two)
  (lambda (x) (display x) x)) 0)
