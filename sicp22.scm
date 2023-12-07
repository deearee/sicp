(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (reverse l)
  (define (reverse-helper l r)
    (if (null? l)
	r
	(reverse-helper (cdr l) (cons (car l) r))))
  (reverse-helper l (list)))

(define (cc amount coin-values)
  (define (no-more? cs) (null? cs))
  (define (first-denom cs) (car cs))
  (define (except-first-denom cs) (cdr cs))
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denom
		 coin-values))
	    (cc (- amount
		   (first-denom
		    coin-values))
		coin-values)))))

(define (same-parity a . r)
  (define (helper r)
    (cond ((null? r) r)
	  ((= (remainder a 2) (remainder (car r) 2))
	   (cons (car r) (helper (cdr r))))
	  (else (helper (cdr r)))))
  (cons a (helper r)))

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x)) items))

(define (for-each proc items)
  (cond ((null? items) #t)
	(else (proc (car items)) (for-each proc (cdr items)))))
