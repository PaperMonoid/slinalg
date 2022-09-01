(define (v-it f a)
  (let loop ((i 0))
    (when (< i (vector-length a))
      (begin
	(f i)
	(loop (1+ i))))))


(define (v-reduce f a e)
  (let loop ((i 0)
	     (r e))
    (if (< i (vector-length a))
	(loop (1+ i) (f (vector-ref a i) r))
	r)))


(define (assert-same-length a b)
  (when (not (= (vector-length a) (vector-length b)))
    (raise "Vectors must have same length.")))


(define (v-map f a)
  (let ((b (make-vector (vector-length a))))
    (v-it (lambda (i) (vector-set! b i (f (vector-ref a i)))) a)
    b))


(define (v-pairwise-map f a b)
  (assert-same-length a b)
  (let ((c (make-vector (vector-length a))))
    (v-it (lambda (i) (vector-set! c i (f (vector-ref a i) (vector-ref b i)))) a)
    c))


(define (v+ a b)
  (cond
   ((number? a) (v-map (lambda (x) (+ a x)) b))
   ((number? b) (v-map (lambda (x) (+ x b)) a))
   (else (v-pairwise-map + a b))))


(define (v- a b)
  (cond
   ((number? a) (v-map (lambda (x) (- a x)) b))
   ((number? b) (v-map (lambda (x) (- x b)) a))
   (else (v-pairwise-map + a b))))


(define (v* a b)
  (cond
   ((number? a) (v-map (lambda (x) (* a x)) b))
   ((number? b) (v-map (lambda (x) (* x b)) a))
   (else (v-pairwise-map * a b))))


(define (v/ a b)
  (cond
   ((number? a) (v-map (lambda (x) (/ a x)) b))
   ((number? b) (v-map (lambda (x) (/ x b)) a))
   (else (v-pairwise-map / a b))))


(define (v-dot a b)
  (assert-same-length a b)
  (let ((c (v.* a b)))
    (v-reduce + c 0)))
