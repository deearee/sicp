(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(define (split compos spli)
  (define (split-helper painter n)
    (if (= n 0)
	painter
	(let ((smaller (split-helper painter (- n 1))))
	  (compos painter (spli smaller smaller))))))

;(define right-split (split beside below))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (x-vect v) (edge1-frame frame))
	       (scale-vect (y-vect v) (edge2-frame frame))))))

;; Vectors

(define (make-vect x y) (list x y))
(define x-vect car)
(define y-vect cadr)

(define (add-vect v1 v2)
  (make-vect (+ (x-vect v1) (x-vect v2))
	     (+ (y-vect v1) (y-vect v2))))

(define (sub-vect v1 v2) (add-vect v1 (scale-vect -1 v2)))

(define (scale-vect a v)
  (make-vect (* a (x-vect v)) (* a (y-vect v))))

;; Frames

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;; Segs

(define (make-seg s e) (list s e))
(define start-seg car)
(define end-seg cadr)

(define (segments->painter seg-list)
  (lambda (frame)
    (for-each
     (lambda (seg)
       (draw-line
	((frame-coord-map frame) (start-seg seg))
	((frame-coord-map frame) (end-seg seg))))
     seg-list)))

(define paint-x
  (segments->painter (list
		      (make-seg (make-vect 0 0) (make-vect 1 1))
		      (make-seg (make-vect 0 1) (make-vect 1 0)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame
		  new-origin
		  (sub-vect (m corner1) new-origin)
		  (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
