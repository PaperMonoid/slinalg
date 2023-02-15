(library (slinalg)
  (export
   vector-zeros
   vector-ones
   vector-fill
   vector-fold-left
   vector+
   vector-
   vector*
   vector/
   vector-dot
   vector-l1
   vector-l2
   vector-dft
   vector-idft
   make-rng-uniform
   make-rng-normal)
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


  (define (vector-fill n x)
    (make-vector n x))


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
	   (W (lambda (k n) (exp (/ (* -i 2 pi k n) N))))
	   (computation (lambda (k n) (* (vector-ref x n) (W k n)))))
      (letrec ((sum-computation
		(lambda (k n result)
		  (if (< n N)
		      (sum-computation k (+ n 1) (+ result (computation k n)))
		      result)))
	       (next-element
		(lambda (k)
		  (if (< k N)
		      (begin
			(vector-set! X k (sum-computation k 0 0))
			(next-element (+ k 1)))
		      X))))
	(next-element 0))))


  (define (vector-idft x)
    (let* ((N (vector-length x))
	   (X (vector-zeros N))
	   (W (lambda (k n) (exp (/ (* 0+i 2 pi k n) N))))
	   (computation (lambda (k n) (* (vector-ref x n) (W k n)))))
      (letrec ((sum-computation
		(lambda (k n result)
		  (if (< n N)
		      (sum-computation k (+ n 1) (+ result (computation k n)))
		      result)))
	       (next-element
		(lambda (k)
		  (if (< k N)
		      (begin
			(vector-set! X k (sum-computation k 0 0))
			(next-element (+ k 1)))
		      X))))
	(next-element 0))))


  (define (make-rng-uniform seed)
    (let ((a 25214903917)
	  (c 11)
	  (m (expt 2 48))
	  (x seed))
      (lambda ()
	(begin
	  (set! x (mod (+ (* a x) c) m))
	  (inexact (/ x m))))))


  (define (box-muller-transform mu sigma u1 u2)
    (let ((magnitude (* sigma (sqrt (* -2 (log u1))))))
      (list
       (+ (* magnitude (cos (* 2 pi u2))) mu)
       (+ (* magnitude (sin (* 2 pi u2))) mu))))


  (define (make-rng-normal seed mu sigma)
    (let ((rng (make-rng-uniform seed)) (z '()))
      (lambda ()
	(begin
	  (when (null? z)
	    (set! z (box-muller-transform mu sigma (rng) (rng))))
	  (let ((z0 (car z)) (z1 (cdr z)))
	    (begin (set! z z1) z0)))))))
