(library (slinalg random)
  (export
   make-rng-uniform
   make-rng-normal)
  (import (rnrs))


  (define pi (* 4 (atan 1)))


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
