;;;
;;; Test eigenmat
;;;

(use gauche.test)
(use gauche.array)

(define (nearly=? x y :optional (abs-tol 1e-4))
  (<= (abs (- x y)) abs-tol))

(test-start "eigenmat")
(use eigenmat)
(test-module 'eigenmat)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-eigenmat" "eigenmat is working"
       (test-eigenmat))

(define A (f64array (shape 0 2 0 2) 1 2 3 4))
(define B (f64array (shape 0 2 0 2) 5 6 7 8))
(define C (f64array (shape 0 2 0 2) 1 2 3 4))
(define D (f64array (shape 0 2 0 2) 1 1 1 1))
(define E (f64array (shape 0 2 0 1) 1 2))

(test* "eigen-array-nearly=? 1" #t (eigen-array-nearly=? A C))
(test* "eigen-array-nearly=? 2" #f (eigen-array-nearly=? A D))

(test* "eigen-array-mul" #,(<f64array> (0 2 0 2) 19 22 43 50)
       (eigen-array-mul A B) eigen-array-nearly=?)

(test* "eigen-array-determinant 1" -2 (eigen-array-determinant A) nearly=?)
(test* "eigen-array-determinant 2"  0 (eigen-array-determinant D) nearly=?)

(test* "eigen-array-inverse 1" #,(<f64array> (0 2 0 2) -2 1 1.5 -0.5)
       (eigen-array-inverse A) eigen-array-nearly=?)
(test* "eigen-array-inverse 2" #,(<f64array> (0 2 0 2) +inf.0 -inf.0 -inf.0 +inf.0)
       (eigen-array-inverse D))

(test* "eigen-array-solve 1" #,(<f64array> (0 2 0 1) 0 0.5)
       (eigen-array-solve A E) eigen-array-nearly=?)
(test* "eigen-array-solve 2" #,(<f64array> (0 2 0 1) -inf.0 +inf.0)
       (eigen-array-solve D E))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

