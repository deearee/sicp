(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* (square x) a) (* x b) c)))

;; sqrt(2)
(newtons-method (cubic 0 -2 0) 3)

(define (double f)
  (lambda (x) (f (f x))))
(define (inc x) (+ x 1))
(((double (double double)) inc) 0)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 10)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (floor-log2 x)
  (floor (/ (log x) (log 2))))

(define (root x n)
  (fixed-point
   ((repeated average-damp (floor-log2 n))
    (lambda (y) (/ x ((repeated (lambda (z) (* y z)) (- n 1)) 1.0))))
   1.0))

(define (iterative-improve good-enough? improve)
  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))
  try)

(define (fixed-point f x)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) dx)) f) x))
