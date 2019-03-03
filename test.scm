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
      (rlet1 ar (make-f64array shape 0)
        (slot-set! ar 'backing-storage (list->f64vector inits))))))

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
(define J (f64array (shape 0 4 0 4) 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
                                    0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6))
(define K (f64array (shape 0 2 0 3) 1 2 3 4 5 6))
(define L (f64array (shape 0 2 0 3) -2 -1 0 1 2 3))
(define M (f64array (shape 0 3 0 2) 1 2 3 4 5 6))

(test* "eigen-make-array 1" F
       (eigen-make-array 0 2 0 2))
(test* "eigen-make-array 2" #,(<f64array> (0 2 0 3) 0 0 0 0 0 0)
       (eigen-make-array 0 2 0 3))
(test* "eigen-make-array 3" #,(<f64array> (0 2 0 3) 0 0 0 0 0 0)
       (rlet1 ar #f
         (eigen-array-cache-off)
         (set! ar (eigen-make-array 0 2 0 3))
         (eigen-array-cache-on)))
(test* "eigen-make-array 4" G
       (eigen-make-array 0 0 0 0))

(test* "eigen-array 1" A
       (eigen-array 0 2 0 2 1 2 3 4))
(test* "eigen-array 2" K
       (eigen-array 0 2 0 3 1 2 3 4 5 6))
(test* "eigen-array 3" K
       (rlet1 ar #f
         (eigen-array-cache-off)
         (set! ar (eigen-array 0 2 0 3 1 2 3 4 5 6))
         (eigen-array-cache-on)))
(test* "eigen-array 4" G
       (eigen-array 0 0 0 0))

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
(test* "eigen-array-add 1-3" G
       (eigen-array-add G G) eigen-array-nearly=?)

(test* "eigen-array-add 2-1" #,(<f64array> (0 2 0 2) 2 3 4 5)
       (eigen-array-add A 1)   eigen-array-nearly=?)
(test* "eigen-array-add 2-2" #,(<f64array> (0 2 0 2) 1.5 1.5 1.5 1.5)
       (eigen-array-add D 0.5) eigen-array-nearly=?)
(test* "eigen-array-add 2-3" G
       (eigen-array-add G 1)   eigen-array-nearly=?)

(test* "eigen-array-add! 1-1" #,(<f64array> (0 2 0 2) 6 8 10 12)
       (eigen-array-add! (eigen-make-array 0 2 0 2) A B) eigen-array-nearly=?)
(test* "eigen-array-add! 1-2" #,(<f64array> (0 2 0 2) 2 2 2 2)
       (eigen-array-add! (eigen-make-array 0 2 0 2) D D) eigen-array-nearly=?)
(test* "eigen-array-add! 1-3" (test-error <error>)
       (eigen-array-add! (eigen-make-array 0 1 0 1) A B))
(test* "eigen-array-add! 1-4" G
       (eigen-array-add! (eigen-make-array 0 0 0 0) G G) eigen-array-nearly=?)

(test* "eigen-array-add! 2-1" #,(<f64array> (0 2 0 2) 2 3 4 5)
       (eigen-array-add! (eigen-make-array 0 2 0 2) A 1)   eigen-array-nearly=?)
(test* "eigen-array-add! 2-2" #,(<f64array> (0 2 0 2) 1.5 1.5 1.5 1.5)
       (eigen-array-add! (eigen-make-array 0 2 0 2) D 0.5) eigen-array-nearly=?)
(test* "eigen-array-add! 2-3" (test-error <error>)
       (eigen-array-add! (eigen-make-array 0 1 0 1) A 1))
(test* "eigen-array-add! 2-4" G
       (eigen-array-add! (eigen-make-array 0 0 0 0) G 1)   eigen-array-nearly=?)

(test* "eigen-array-sub 1-1" #,(<f64array> (0 2 0 2) -4 -4 -4 -4)
       (eigen-array-sub A B) eigen-array-nearly=?)
(test* "eigen-array-sub 1-2" #,(<f64array> (0 2 0 2) 0 0 0 0)
       (eigen-array-sub D D) eigen-array-nearly=?)
(test* "eigen-array-sub 1-3" G
       (eigen-array-sub G G) eigen-array-nearly=?)

(test* "eigen-array-sub 2-1" #,(<f64array> (0 2 0 2) 0 1 2 3)
       (eigen-array-sub A 1)   eigen-array-nearly=?)
(test* "eigen-array-sub 2-2" #,(<f64array> (0 2 0 2) 0.5 0.5 0.5 0.5)
       (eigen-array-sub D 0.5) eigen-array-nearly=?)
(test* "eigen-array-sub 2-3" G
       (eigen-array-sub G 1)   eigen-array-nearly=?)

(test* "eigen-array-sub! 1-1" #,(<f64array> (0 2 0 2) -4 -4 -4 -4)
       (eigen-array-sub! (eigen-make-array 0 2 0 2) A B) eigen-array-nearly=?)

(test* "eigen-array-sub! 2-1" #,(<f64array> (0 2 0 2) 0 1 2 3)
       (eigen-array-sub! (eigen-make-array 0 2 0 2) A 1) eigen-array-nearly=?)

(test* "eigen-array-mul 1" #,(<f64array> (0 2 0 2) 19 22 43 50)
       (eigen-array-mul A B) eigen-array-nearly=?)
(test* "eigen-array-mul 2" #,(<f64array> (0 2 0 2) 2 2 2 2)
       (eigen-array-mul D D) eigen-array-nearly=?)
(test* "eigen-array-mul 3" #,(<f64array> (0 2 0 2) 22 28 49 64)
       (eigen-array-mul K M) eigen-array-nearly=?)
(test* "eigen-array-mul 4" G
       (eigen-array-mul G G) eigen-array-nearly=?)

(test* "eigen-array-mul! 1" #,(<f64array> (0 2 0 2) 19 22 43 50)
       (eigen-array-mul! (eigen-make-array 0 2 0 2) A B) eigen-array-nearly=?)

(test* "eigen-array-mul-elements 1-1" #,(<f64array> (0 2 0 2) 5 12 21 32)
       (eigen-array-mul-elements A B) eigen-array-nearly=?)
(test* "eigen-array-mul-elements 1-2" D
       (eigen-array-mul-elements D D) eigen-array-nearly=?)
(test* "eigen-array-mul-elements 1-3" G
       (eigen-array-mul-elements G G) eigen-array-nearly=?)

(test* "eigen-array-mul-elements 2-1" #,(<f64array> (0 2 0 2) 2 4 6 8)
       (eigen-array-mul-elements A 2)   eigen-array-nearly=?)
(test* "eigen-array-mul-elements 2-2" #,(<f64array> (0 2 0 2) 0.5 0.5 0.5 0.5)
       (eigen-array-mul-elements D 0.5) eigen-array-nearly=?)
(test* "eigen-array-mul-elements 2-3" G
       (eigen-array-mul-elements G 2)   eigen-array-nearly=?)

(test* "eigen-array-mul-elements! 1-1" #,(<f64array> (0 2 0 2) 5 12 21 32)
       (eigen-array-mul-elements! (eigen-make-array 0 2 0 2) A B) eigen-array-nearly=?)

(test* "eigen-array-mul-elements! 2-1" #,(<f64array> (0 2 0 2) 2 4 6 8)
       (eigen-array-mul-elements! (eigen-make-array 0 2 0 2) A 2) eigen-array-nearly=?)

(test* "eigen-array-div 1" #,(<f64array> (0 2 0 2) 0.5 1.0 1.5 2.0)
       (eigen-array-div A 2) eigen-array-nearly=?)
(test* "eigen-array-div 2" #,(<f64array> (0 2 0 2) +inf.0 +inf.0 +inf.0 +inf.0)
       (eigen-array-div A 0))
(test* "eigen-array-div 3" G
       (eigen-array-div G 2) eigen-array-nearly=?)

(test* "eigen-array-div! 1" #,(<f64array> (0 2 0 2) 0.5 1.0 1.5 2.0)
       (eigen-array-div! (eigen-make-array 0 2 0 2) A 2) eigen-array-nearly=?)

(test* "eigen-array-pow 1" #,(<f64array> (0 2 0 2) 1 4 9 16)
       (eigen-array-pow A 2) eigen-array-nearly=?)
(test* "eigen-array-pow 2" (f64array (shape 0 2 0 2) 1 (sqrt 2) (sqrt 3) 2)
       (eigen-array-pow A 0.5) eigen-array-nearly=?)
(test* "eigen-array-pow 3" D
       (eigen-array-pow A 0) eigen-array-nearly=?)
(test* "eigen-array-pow 4" G
       (eigen-array-pow G 2) eigen-array-nearly=?)

(test* "eigen-array-pow! 1" #,(<f64array> (0 2 0 2) 1 4 9 16)
       (eigen-array-pow! (eigen-make-array 0 2 0 2) A 2) eigen-array-nearly=?)

(test* "eigen-array-exp 1" (f64array (shape 0 2 0 2) (exp 1) (exp 2) (exp 3) (exp 4))
       (eigen-array-exp A) eigen-array-nearly=?)
(test* "eigen-array-exp 2" D
       (eigen-array-exp F) eigen-array-nearly=?)
(test* "eigen-array-exp 3" G
       (eigen-array-exp G) eigen-array-nearly=?)

(test* "eigen-array-exp! 1" (f64array (shape 0 2 0 2) (exp 1) (exp 2) (exp 3) (exp 4))
       (eigen-array-exp! (eigen-make-array 0 2 0 2) A) eigen-array-nearly=?)

(test* "eigen-array-log 1" (f64array (shape 0 2 0 2) (log 1) (log 2) (log 3) (log 4))
       (eigen-array-log A) eigen-array-nearly=?)
(test* "eigen-array-log 2" #,(<f64array> (0 2 0 2) -inf.0 -inf.0 -inf.0 -inf.0)
       (eigen-array-log F))
(test* "eigen-array-log 3" G
       (eigen-array-log G) eigen-array-nearly=?)

(test* "eigen-array-log! 1" (f64array (shape 0 2 0 2) (log 1) (log 2) (log 3) (log 4))
       (eigen-array-log! (eigen-make-array 0 2 0 2) A) eigen-array-nearly=?)

(test* "eigen-array-sigmoid 1"
       (let ()
         (define (sigmoid x) (/. 1 (+ 1 (exp (- x)))))
         (f64array (shape 0 2 0 2) (sigmoid 1) (sigmoid 2) (sigmoid 3) (sigmoid 4)))
       (eigen-array-sigmoid A) eigen-array-nearly=?)
(test* "eigen-array-sigmoid 2" #,(<f64array> (0 2 0 2) 0.5 0.5 0.5 0.5)
       (eigen-array-sigmoid F))
(test* "eigen-array-sigmoid 3" G
       (eigen-array-sigmoid G) eigen-array-nearly=?)

(test* "eigen-array-sigmoid! 1"
       (let ()
         (define (sigmoid x) (/. 1 (+ 1 (exp (- x)))))
         (f64array (shape 0 2 0 2) (sigmoid 1) (sigmoid 2) (sigmoid 3) (sigmoid 4)))
       (eigen-array-sigmoid! (eigen-make-array 0 2 0 2) A) eigen-array-nearly=?)

(test* "eigen-array-relu 1" A
       (eigen-array-relu A) eigen-array-nearly=?)
(test* "eigen-array-relu 2" #,(<f64array> (0 2 0 3) 0 0 0 1 2 3)
       (eigen-array-relu L) eigen-array-nearly=?)
(test* "eigen-array-relu 3" G
       (eigen-array-relu G) eigen-array-nearly=?)

(test* "eigen-array-relu! 1" A
       (eigen-array-relu! (eigen-make-array 0 2 0 2) A) eigen-array-nearly=?)

(test* "eigen-array-step 1" D
       (eigen-array-step A) eigen-array-nearly=?)
(test* "eigen-array-step 2" #,(<f64array> (0 2 0 3) 0 0 0 1 1 1)
       (eigen-array-step L) eigen-array-nearly=?)
(test* "eigen-array-step 3" G
       (eigen-array-step G) eigen-array-nearly=?)

(test* "eigen-array-step! 1" D
       (eigen-array-step! (eigen-make-array 0 2 0 2) A) eigen-array-nearly=?)

(test* "eigen-array-sum 1" 10 (eigen-array-sum A) nearly=?)
(test* "eigen-array-sum 2" 0  (eigen-array-sum G) nearly=?)

(test* "eigen-array-min 1" 1  (eigen-array-min A) nearly=?)
(test* "eigen-array-min 2" (test-error <error>) (eigen-array-min G))

(test* "eigen-array-max 1" 4  (eigen-array-max A) nearly=?)
(test* "eigen-array-max 2" (test-error <error>) (eigen-array-max G))

(test* "eigen-array-mean 1" 2.5 (eigen-array-mean A) nearly=?)
(test* "eigen-array-mean 2" (test-error <error>) (eigen-array-mean G))

(test* "eigen-array-trace 1" 5  (eigen-array-trace A) nearly=?)
(test* "eigen-array-trace 2" 0  (eigen-array-trace G) nearly=?)

(test* "eigen-array-determinant 1" -2 (eigen-array-determinant A) nearly=?)
(test* "eigen-array-determinant 2"  0 (eigen-array-determinant D) nearly=?)
(test* "eigen-array-determinant 3"  1 (eigen-array-determinant G) nearly=?)

(test* "eigen-array-transpose 1" #,(<f64array> (0 2 0 2) 1 3 2 4)
       (eigen-array-transpose A))
(test* "eigen-array-transpose 2" #,(<f64array> (0 3 0 2) 1 4 2 5 3 6)
       (eigen-array-transpose K))
(test* "eigen-array-transpose 3" G
       (eigen-array-transpose G))

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

(test* "eigen-array-row 1" #,(<f64array> (0 1 0 4) 1 2 3 4)
       (eigen-array-row H 0))
(test* "eigen-array-row 2" #,(<f64array> (0 1 0 3) 4 5 6)
       (eigen-array-row K 1))
(test* "eigen-array-row 3" (test-error <error>)
       (eigen-array-row H -1))
(test* "eigen-array-row 4" (test-error <error>)
       (eigen-array-row H 4))
(test* "eigen-array-row 5" (test-error <error>)
       (eigen-array-row G 0))

(test* "eigen-array-col 1" #,(<f64array> (0 4 0 1) 1 5 9 13)
       (eigen-array-col H 0))
(test* "eigen-array-col 2" #,(<f64array> (0 2 0 1) 3 6)
       (eigen-array-col K 2))
(test* "eigen-array-col 3" (test-error <error>)
       (eigen-array-col H -1))
(test* "eigen-array-col 4" (test-error <error>)
       (eigen-array-col H 4))
(test* "eigen-array-col 5" (test-error <error>)
       (eigen-array-col G 0))

(test* "eigen-array-block 1-1" #,(<f64array> (0 2 0 2) 1 2 5 6)
       (eigen-array-block H 0 0 2 2))
(test* "eigen-array-block 1-2" #,(<f64array> (0 2 0 3) 6 7 8 10 11 12)
       (eigen-array-block H 1 1 2 3))
(test* "eigen-array-block 1-3" #,(<f64array> (0 1 0 2) 15 16)
       (eigen-array-block H 3 2 1 2))
(test* "eigen-array-block 1-4" #,(<f64array> (0 2 0 2) 1 2 4 5)
       (eigen-array-block K 0 0 2 2))
(test* "eigen-array-block 1-5" #,(<f64array> (0 1 0 2) 5 6)
       (eigen-array-block K 1 1 1 2))
(test* "eigen-array-block 1-6" (test-error <error>)
       (eigen-array-block H -1 -1 2 2))
(test* "eigen-array-block 1-7" (test-error <error>)
       (eigen-array-block H 3 3 2 2))
(test* "eigen-array-block 1-8" G
       (eigen-array-block H 0 0 0 0))
(test* "eigen-array-block 1-9" G
       (eigen-array-block G 0 0 0 0))
(test* "eigen-array-block 1-10" (test-error <error>)
       (eigen-array-block G 0 0 1 1))

(test* "eigen-array-block 2-1" #,(<f64array> (0 4 0 4) 1   2   0.3 0.4 5   6   0.7 0.8
                                                       0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6)
       (eigen-array-block H 0 0 2 2 J 0 0))
(test* "eigen-array-block 2-2" #,(<f64array> (0 4 0 4) 0.1 0.2 0.3 0.4 0.5 6   7   8
                                                       0.9 10  11  12  1.3 1.4 1.5 1.6)
       (eigen-array-block H 1 1 2 3 J 1 1))
(test* "eigen-array-block 2-3" #,(<f64array> (0 4 0 4) 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
                                                       0.9 1.0 1.1 1.2 1.3 1.4 15  16)
       (eigen-array-block H 3 2 1 2 J 3 2))
(test* "eigen-array-block 2-4" #,(<f64array> (0 2 0 3) -2 1 2 1 4 5)
       (eigen-array-block K 0 0 2 2 L 0 1))
(test* "eigen-array-block 2-5" #,(<f64array> (0 2 0 3) -2 5 6 1 2 3)
       (eigen-array-block K 1 1 1 2 L 0 1))
(test* "eigen-array-block 2-6" (test-error <error>)
       (eigen-array-block H -1 -1 2 2 J 0 0))
(test* "eigen-array-block 2-7" (test-error <error>)
       (eigen-array-block H 3 3 2 2 J 0 0))
(test* "eigen-array-block 2-8" (test-error <error>)
       (eigen-array-block H 0 0 2 2 J -1 -1))
(test* "eigen-array-block 2-9" (test-error <error>)
       (eigen-array-block H 0 0 2 2 J 3 3))
(test* "eigen-array-block 2-10" J
       (eigen-array-block H 0 0 0 0 J 0 0))
(test* "eigen-array-block 2-11" J
       (eigen-array-block G 0 0 0 0 J 0 0))
(test* "eigen-array-block 2-12" (test-error <error>)
       (eigen-array-block G 0 0 1 1 J 0 0))
(test* "eigen-array-block 2-13" G
       (eigen-array-block H 0 0 0 0 G 0 0))
(test* "eigen-array-block 2-14" (test-error <error>)
       (eigen-array-block H 0 0 0 0 G 1 1))

(format (current-error-port) "~%~a" ((with-module gauche.test format-summary)))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)

