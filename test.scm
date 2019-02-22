;;;
;;; Test eigenmat
;;;

(use gauche.test)
(use gauche.array)
(use gauche.uvector)

(define (nearly=? x y :optional (precision 1e-12))
  (<= (abs (- x y)) precision))

;; for Gauche v0.9.4
(define f64array
  (if (global-variable-bound? 'gauche.array 'f64array)
    (with-module gauche.array f64array)
    (lambda (shape . inits)
      (rlet1 arr (make-f64array shape 0)
        (slot-set! arr 'backing-storage
                   (vector->f64vector
                    (slot-ref (apply array shape inits) 'backing-storage)))))))

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
(define F (f64array (shape 0 2 0 2) 0 0 0 0))
(define G (f64array (shape 0 0 0 0)))
(define H (f64array (shape 0 4 0 4) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

(test* "eigen-array-nearly=? 1" #t (eigen-array-nearly=? A A))
(test* "eigen-array-nearly=? 2" #t (eigen-array-nearly=? A C))
(test* "eigen-array-nearly=? 3" #f (eigen-array-nearly=? A D))
(test* "eigen-array-nearly=? 4" #t (eigen-array-nearly=? G G))

(test* "eigen-array-nearly-zero? 1" #t (eigen-array-nearly-zero? F))
(test* "eigen-array-nearly-zero? 2" #f (eigen-array-nearly-zero? D))
(test* "eigen-array-nearly-zero? 3" #t (eigen-array-nearly-zero? G))

(test* "eigen-array-add 1-1" #,(<f64array> (0 2 0 2) 6 8 10 12)
       (eigen-array-add A B) eigen-array-nearly=?)
(test* "eigen-array-add 1-2" #,(<f64array> (0 2 0 2) 2 2 2 2)
       (eigen-array-add D D) eigen-array-nearly=?)
(test* "eigen-array-add 1-3" #,(<f64array> (0 0 0 0))
       (eigen-array-add G G) eigen-array-nearly=?)

(test* "eigen-array-add 2-1" #,(<f64array> (0 2 0 2) 2 3 4 5)
       (eigen-array-add A 1)   eigen-array-nearly=?)
(test* "eigen-array-add 2-2" #,(<f64array> (0 2 0 2) 1.5 1.5 1.5 1.5)
       (eigen-array-add D 0.5) eigen-array-nearly=?)
(test* "eigen-array-add 2-3" #,(<f64array> (0 0 0 0))
       (eigen-array-add G 1)   eigen-array-nearly=?)

(test* "eigen-array-sub 1-1" #,(<f64array> (0 2 0 2) -4 -4 -4 -4)
       (eigen-array-sub A B) eigen-array-nearly=?)
(test* "eigen-array-sub 1-2" #,(<f64array> (0 2 0 2) 0 0 0 0)
       (eigen-array-sub D D) eigen-array-nearly=?)
(test* "eigen-array-sub 1-3" #,(<f64array> (0 0 0 0))
       (eigen-array-sub G G) eigen-array-nearly=?)

(test* "eigen-array-sub 2-1" #,(<f64array> (0 2 0 2) 0 1 2 3)
       (eigen-array-sub A 1)   eigen-array-nearly=?)
(test* "eigen-array-sub 2-2" #,(<f64array> (0 2 0 2) 0.5 0.5 0.5 0.5)
       (eigen-array-sub D 0.5) eigen-array-nearly=?)
(test* "eigen-array-sub 2-3" #,(<f64array> (0 0 0 0))
       (eigen-array-sub G 1)   eigen-array-nearly=?)

(test* "eigen-array-mul 1-1" #,(<f64array> (0 2 0 2) 19 22 43 50)
       (eigen-array-mul A B) eigen-array-nearly=?)
(test* "eigen-array-mul 1-2" #,(<f64array> (0 2 0 2) 2 2 2 2)
       (eigen-array-mul D D) eigen-array-nearly=?)
(test* "eigen-array-mul 1-3" #,(<f64array> (0 0 0 0))
       (eigen-array-mul G G) eigen-array-nearly=?)

(test* "eigen-array-mul 2-1" #,(<f64array> (0 2 0 2) 2 4 6 8)
       (eigen-array-mul A 2)   eigen-array-nearly=?)
(test* "eigen-array-mul 2-2" #,(<f64array> (0 2 0 2) 0.5 0.5 0.5 0.5)
       (eigen-array-mul D 0.5) eigen-array-nearly=?)
(test* "eigen-array-mul 2-3" #,(<f64array> (0 0 0 0))
       (eigen-array-mul G 2)   eigen-array-nearly=?)

(test* "eigen-array-determinant 1" -2 (eigen-array-determinant A) nearly=?)
(test* "eigen-array-determinant 2"  0 (eigen-array-determinant D) nearly=?)
(test* "eigen-array-determinant 3"  1 (eigen-array-determinant G) nearly=?)

(test* "eigen-array-inverse 1" #,(<f64array> (0 2 0 2) -2 1 1.5 -0.5)
       (eigen-array-inverse A) eigen-array-nearly=?)
(test* "eigen-array-inverse 2" #,(<f64array> (0 2 0 2) +inf.0 -inf.0 -inf.0 +inf.0)
       (eigen-array-inverse D))
(test* "eigen-array-inverse 3" (test-error <error>)
       (eigen-array-inverse G))

(test* "eigen-array-solve 1" #,(<f64array> (0 2 0 1) 0 0.5)
       (eigen-array-solve A E) eigen-array-nearly=?)
(test* "eigen-array-solve 2" #,(<f64array> (0 2 0 1) -inf.0 +inf.0)
       (eigen-array-solve D E))
(test* "eigen-array-solve 3" (test-error <error>)
       (eigen-array-solve G G))

(test* "eigen-array-block 1" #,(<f64array> (0 2 0 2) 1 2 5 6)
       (eigen-array-block H 0 0 2 2))
(test* "eigen-array-block 2" #,(<f64array> (0 2 0 3) 6 7 8 10 11 12)
       (eigen-array-block H 1 1 2 3))
(test* "eigen-array-block 3" #,(<f64array> (0 1 0 2) 15 16)
       (eigen-array-block H 3 2 1 2))
(test* "eigen-array-block 4" (test-error <error>)
       (eigen-array-block H -1 -1 2 2))
(test* "eigen-array-block 5" (test-error <error>)
       (eigen-array-block H 3 3 2 2))
(test* "eigen-array-block 6" #,(<f64array> (0 0 0 0))
       (eigen-array-block H 0 0 0 0))
(test* "eigen-array-block 7" #,(<f64array> (0 0 0 0))
       (eigen-array-block G 0 0 0 0))
(test* "eigen-array-block 8" (test-error <error>)
       (eigen-array-block G 0 0 1 1))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

