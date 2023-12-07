(define (f n)
     (if (< n 3)
	 n
	 (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (fi n)
  (define (fiter a b c count)
    (if (> count 0)
	(fiter b c (+ c (* 2 b) (* 3 a)) (- count 1))
	a))
  (fiter 0 1 2 n))

(define (pascal n k)
  (if (or (= k 0) (= k n))
      1
      (+ (pascal (- n 1) k) (pascal (- n 1) (- k 1)))))

(define (fast-exp b n)
  (if (= n 0)
      1
      (if (even? n)
	  (square (fast-exp b (/ n 2)))
	  (* b (fast-exp b (- n 1))))))

(define (halve x) (/ x 2))
(define (double x) (+ x x))

(define (fast-mult a b)
  (if (= b 0)
      0
      (if (even? b)
	  (double (fast-mult a (halve b)))
	  (+ a (fast-mult a (- b 1))))))
