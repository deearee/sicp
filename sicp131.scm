(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (sum-squares n)
  (sum square 1 inc n))

(define (simpson-integral f a b n)
  (define (4or2 k) (if (even? k) 2 4))
  (define (term k)
    (* (4or2 k)
       (f (+ a
	     (* k (/ (- b a)
		     n))))))
  (* (/ (- b a) n 3)
     (+ (f a)
	(f b)
	(sum term 1 inc (- n 1)))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (pi-product n)
  (define (term n)
    (square (/ (* n 2)
	       (+ n n -1))))
  (* (product-acc term 1.0 inc n) 2 (/ 1.0 (+ n n 1))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner
	   (term a)
	   (filtered-accumulate filter combiner null-value term (next a) next b))
	  (filtered-accumulate filter combiner null-value term (next a) next b))))
