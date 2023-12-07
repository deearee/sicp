(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (cont-frac m)
    (if (> m k)
	0
	(/ (n m)
	   (+ (d m)
	      (cont-frac (+ m 1))))))
  (cont-frac 1))

;; 1/phi
(/ 1.0 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; 1/phi
(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   11)

;; e-2
(cont-frac (lambda (i) 1.0)
	   (lambda (i)
	     (define k (+ i 4))
	     (if (= (remainder k 3) 0) (/ k 3) 1.0))
	   1000)

(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (- (square x)))
		(lambda (i) (- (* 2 i) 1))
		k) (- x)))

;; sqrt(3)
(tan-cf (/ 3.141592 3) 100)
