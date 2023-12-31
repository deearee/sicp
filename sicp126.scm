(define (smallest-divisor n)
  (define (test-divisor x)
    (cond ((> (square x) n) n)
	  ((divides? x) x)
	  (else (test-divisor (+ x 2)))))
  (define (divides? x)
    (= (remainder n x) 0))
  (define (next x)
    (if (= x 2) 3 (+ x 2)))
  (if (divides? 2) 2 (test-divisor 3)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5) (report-prime n (- (runtime) start-time)) #f))

(define (report-prime n elapsed-time)
  (display " *** ")
  (display n)
  (display " ")
  (display elapsed-time)
  (newline)
  #t)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (search-for-primes s)
  (define (searcher n c)
    (cond ((>= c 3) #f)
	  ((timed-prime-test (+ s n)) (searcher (+ n 1) (+ c 1)))
	  (else (searcher (+ n 1) c))))
  (searcher 1 0))

(define (expmod b e m)
  (cond ((= e 0) 1)
	((even? e)
	 (remainder
	  (square (expmod b (/ e 2) m))
	  m))
	(else
	 (remainder
	  (* b (expmod b (- e 1) m))
	  m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n t)
  (cond ((= t 0) true)
	((fermat-test n) (fast-prime? n (- t 1)))
	(else false)))

(define (carmichael n)
  (define (trymichael x)
    (cond ((>= x n) #t)
	  ((= (expmod x n n) x) (trymichael (+ x 1)))
	  (else #f)))
  (trymichael 0))

(define (miller-rabin-test-all n)
  (define (try x)
    (cond ((>= x n) #t)
	  ((not (miller-rabin-test n x)) (try (+ x 1)))
	  (else (display x) (newline) (try (+ x 1)))))
  (try 2))

(define (miller-rabin-test n a)
  (define (check-square x s)
    (cond ((or (= x 1) (= x (- n 1))) s)
	  ((= s 1) 0)
	  (else s)))
  (define (signal-square x m)
    (check-square x (remainder (square x) m)))
  (define (expmod b e m)
    (cond ((= e 0) 1)
	  ((even? e)
	   (signal-square (expmod b (/ e 2) m) m))
	  (else
	   (remainder
	    (* b (expmod b (- e 1) m))
	    m))))
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it a))

(define (miller-rabin n)
  (miller-rabin-test (+ 1 (random (- n 1)))))
