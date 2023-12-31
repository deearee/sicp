; Exercise 3.1
(define (make-accumulator sum)
  (lambda (inc)
    (begin (set! sum (+ sum inc))
	   sum)))

; Exercise 3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) calls)
	    ((eq? x 'reset-calls) (set! calls 0))
	    (else (begin (set! calls (+ calls 1))
			 (f x)))))))

; Exercise 3.3 and 3.4
(define (make-account balance pass)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define bad-logins 0)
  (define (call-the-cops)
    "Calling the cops")
  (define (wrong-pass amount)
    (begin (set! bad-logins (+ bad-logins 1))
	   (if (>= bad-logins 7)
	       (call-the-cops)
	       (string-append
		"Wrong password. Tries left: "
		(number->string (- 7 bad-logins))))))
  (define (dispatch p m)
    (cond ((not (eq? p pass)) wrong-pass)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)
