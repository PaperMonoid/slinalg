(import (slinalg))

(vector+ (vector* 2 '#(1 2 3)) '#(3 2 5))

(vector-l1 '#(3 4 1))

(vector-l2 '#(3 4 1))

(vector-idft (vector-dft '#(11 22 3 4)))

(let ((rng (make-rng-normal 1 0.5 0.2)))
  (display (rng)) (newline)
  (display (rng)) (newline)
  (display (rng)) (newline)
  (display (rng)) (newline)
  (display (rng)) (newline)
  (display (rng)) (newline))
