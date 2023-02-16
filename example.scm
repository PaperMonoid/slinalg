(import
 (slinalg vector)
 (slinalg random))

(display (vector+ (vector* 2 '#(1 2 3)) '#(3 2 5)))
(newline)

(display (vector-l1 '#(3 4 1)))
(newline)

(display (vector-l2 '#(3 4 1)))
(newline)

(display (vector-idft (vector-dft '#(11 22 3 4))))
(newline)

(display (vector-linspace 0 10 5))
(newline)

(display (vector-fill-function 10 (make-rng-normal 1 0.5 0.1)))
(newline)

(display (vector-fill-function 10 (make-rng-uniform 1)))
(newline)
