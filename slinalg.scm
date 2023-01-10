(library (slinalg)
  (export
   vector-fold-left
   vector+
   vector-
   vector*
   vector/
   vector-dot
   vector-l1
   vector-l2
   vector-dft
   vector-idft)
  (import (rnrs))

  (define (for-n f n)
    (let loop ((i 0))
      (when (< i n)
	(begin
	  (f i)
	  (loop (+ 1 i))))))


  (define (vector-fold-left f id x)
    (let loop ((i 0) (result id))
      (if (< i (vector-length x))
	  (loop (+ 1 i) (f (vector-ref x i) result))
	  result)))


  (define (assert-same-length x y)
    (when (not (= (vector-length x) (vector-length y)))
      (raise "Vectors must have same length.")))


  (define (vector-scalar-map f . xs)
    (fold-left
     (lambda (x y)
       (cond
	((and (number? x) (number? y)) (f x y))
	((number? y) (vector-map (lambda (z) (f z y)) x))
	((number? x) (vector-map (lambda (z) (f x z)) y))
	(else (assert-same-length x y) (vector-map f x y))))
     (car xs) (cdr xs)))


  (define (vector-zeros n)
    (make-vector n 0))


  (define (vector-ones n)
    (make-vector n 1))


  (define (vector+ . xs)
    (apply vector-scalar-map + xs))


  (define (vector- . xs)
    (apply vector-scalar-map - xs))


  (define (vector* . xs)
    (apply vector-scalar-map * xs))


  (define (vector/ . xs)
    (apply vector-scalar-map / xs))


  (define (vector-dot x y)
    (assert-same-length x y)
    (let ((z (vector* x y)))
      (vector-fold-left + 0 z)))


  (define (vector-l1 xs)
    (vector-fold-left (lambda (y x) (+ x (abs y))) 0 xs))


  (define (vector-l2 xs)
    (expt (vector-fold-left (lambda (y x) (+ x (expt y 2))) 0 xs) 0.5))


  (define pi (* 4 (atan 1)))


  (define (vector-dft x)
    (let* ((N (vector-length x))
	   (X (vector-zeros N))
	   (calculation
	    (lambda (k n)
	      (* (vector-ref x n) (exp (/ (* -i 2 pi k n) N))))))
      (let next-term ((k 0))
	(if (< k N)
	    (begin
	      (vector-set!
	       X
	       k
	       (let collect-term
		   ((k k) (n 0) (result 0))
		 (if (< n N)
		     (collect-term k (+ n 1) (+ result (calculation k n)))
		     result)))
	      (next-term (+ k 1)))
	    X))))


    (define (vector-idft x)
    (let* ((N (vector-length x))
	   (X (vector-zeros N))
	   (calculation
	    (lambda (k n)
	      (* (vector-ref x n) (exp (/ (* 0+i 2 pi k n) N))))))
      (let next-term ((k 0))
	(if (< k N)
	    (begin
	      (vector-set!
	       X
	       k
	       (let collect-term
		   ((k k) (n 0) (result 0))
		 (if (< n N)
		     (collect-term k (+ n 1) (+ result (calculation k n)))
		     (/ result N))))
	      (next-term (+ k 1)))
	    X)))))
