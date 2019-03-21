;; -*- coding: utf-8 -*-
;;
;; eigenmat.scm
;; 2019-3-21 v1.29
;;
;; ＜内容＞
;;   Gauche で、Eigen ライブラリ を使って行列の高速演算を行うためのモジュールです。
;;   Eigen は、C++ で書かれた線形代数用のライブラリです ( http://eigen.tuxfamily.org )。
;;   現状、本モジュールは、標準の gauhce.array モジュールにおける
;;   2次元の f64array のみ演算が可能です。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/eigenmat
;;
(define-module eigenmat
  (use gauche.uvector)
  (use gauche.array)
  (use gauche.version)
  (export
    test-eigenmat
    eigen-array-cache-on     eigen-array-cache-off
    make-eigen-array         make-eigen-array-same-shape
    eigen-make-array         eigen-make-array-same-shape ; for compatibility
    eigen-array
    eigen-array-nearly=?     eigen-array-nearly-zero?
    eigen-array-add          eigen-array-add!
    eigen-array-sub          eigen-array-sub!
    eigen-array-mul          eigen-array-mul!
    eigen-array-mul-elements eigen-array-mul-elements!
    eigen-array-div          eigen-array-div!
    eigen-array-pow          eigen-array-pow!
    eigen-array-exp          eigen-array-exp!
    eigen-array-log          eigen-array-log!
    eigen-array-sigmoid      eigen-array-sigmoid!
    eigen-array-relu         eigen-array-relu!
    eigen-array-step         eigen-array-step!
    eigen-array-sum
    eigen-array-min
    eigen-array-max
    eigen-array-mean
    eigen-array-trace
    eigen-array-determinant
    eigen-array-transpose    eigen-array-transpose!
    eigen-array-inverse      eigen-array-inverse!
    eigen-array-solve        eigen-array-solve!
    eigen-array-row          eigen-array-row!
    eigen-array-col          eigen-array-col!
    eigen-array-block        eigen-array-block!
    ))
(select-module eigenmat)

;; Loads extension
(dynamic-load "eigenmat")

;;
;; Put your Scheme definitions here
;;

;; == 内部処理用 ==

;; s32vector をハッシュテーブルのキーに使えるようにする
;; (Gauche の開発最新版では、デフォルトで使用可能)
(when (guard (ex (else #t)) (default-hash #s32(1)) #f)
  ;; for Gauche v0.9.4
  (if (version<=? (gauche-version) "0.9.4")
    (define-method object-hash ((obj <s32vector>))
      (hash (s32vector->vector obj)))
    (define-method object-hash ((obj <s32vector>) rec-hash)
      (rec-hash (s32vector->vector obj)))))

;; 行列の情報取得(エラーチェックなし)
(define-syntax array-rank
  (syntax-rules ()
    ((_ A)
     (s32vector-length (slot-ref A 'start-vector)))))
(define-syntax array-start
  (syntax-rules ()
    ((_ A dim)
     (s32vector-ref    (slot-ref A 'start-vector) dim))))
(define-syntax array-end
  (syntax-rules ()
    ((_ A dim)
     (s32vector-ref    (slot-ref A 'end-vector)   dim))))
(define-syntax array-length
  (syntax-rules ()
    ((_ A dim)
     (- (s32vector-ref (slot-ref A 'end-vector)   dim)
        (s32vector-ref (slot-ref A 'start-vector) dim)))))

;; 行列のコピー(エラーチェックなし)
(define (array-copy A)
  (make (class-of A)
    :start-vector    (slot-ref A 'start-vector)
    :end-vector      (slot-ref A 'end-vector)
    :mapper          (slot-ref A 'mapper)
    :backing-storage (let1 v (slot-ref A 'backing-storage)
                       (if (vector? v)
                         (vector-copy v)
                         (uvector-copy v)))))

;; 行列の次元数のチェック
(define-syntax check-array-rank
  (syntax-rules ()
    ((_ A B ...)
     (unless (= (array-rank A) (array-rank B) ... 2)
       (error "array rank must be 2")))))

;; 行列のキャッシュ(ハッシュテーブル)
(define use-eigen-array-cache #t) ; 使用有無
(define array-cache-table (make-hash-table 'equal?))

;; == ここから 公開I/F ==

;; 行列のキャッシュ使用/未使用
(define (eigen-array-cache-on)
  (set! use-eigen-array-cache #t))
(define (eigen-array-cache-off)
  (set! use-eigen-array-cache #f))

;; 行列の生成(キャッシュ使用)
(define (make-eigen-array ns ne ms me . maybe-init)
  (if use-eigen-array-cache
    (let1 key (s32vector ns ne ms me)
      (if-let1 A (hash-table-get array-cache-table key #f)
        (if (or (null? maybe-init) (= (car maybe-init) 0))
          (array-copy A)
          (rlet1 B (array-copy A)
            (f64vector-fill! (slot-ref B 'backing-storage) (car maybe-init))))
        (let1 B (make-f64array (shape ns ne ms me) 0)
          (hash-table-put! array-cache-table key B)
          (if (or (null? maybe-init) (= (car maybe-init) 0))
            (array-copy B)
            (rlet1 C (array-copy B)
              (f64vector-fill! (slot-ref C 'backing-storage) (car maybe-init)))))))
    (apply make-f64array (shape ns ne ms me) maybe-init)))
(define eigen-make-array make-eigen-array)

;; 同じ shape の行列の生成
(define (make-eigen-array-same-shape A . maybe-init)
  (check-array-rank A)
  (let ((ns (array-start A 0))
        (ne (array-end   A 0))
        (ms (array-start A 1))
        (me (array-end   A 1)))
    (apply make-eigen-array ns ne ms me maybe-init)))
(define eigen-make-array-same-shape make-eigen-array-same-shape)

;; 行列の初期化データ付き生成
(define (eigen-array ns ne ms me . inits)
  (rlet1 A (make-eigen-array ns ne ms me 0)
    (f64vector-copy! (slot-ref A 'backing-storage)
                     0 (list->f64vector inits))))

;; 行列の一致チェック
(define-method eigen-array-nearly=? ((A <f64array>)
                                     (B <f64array>)
                                     :optional (precision 1e-12))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "array shape mismatch"))
    (eigen-matrix-nearly-p data1 n1 m1 data2 n2 m2 precision)))

;; 行列のゼロチェック
(define-method eigen-array-nearly-zero? ((A <f64array>)
                                         :optional (precision 1e-12))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (eigen-matrix-nearly-zero-p data1 n1 m1 precision)))

;; 行列の演算生成用マクロ
;; (行列2個の演算)
(define-macro (define-eigen-array-op op1)
  `(define-method ,(symbol-append 'eigen-array- op1) ((A <f64array>) (B <f64array>))
     (check-array-rank A B)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1))
           (data2 (slot-ref B 'backing-storage))
           (n2    (array-length B 0))
           (m2    (array-length B 1)))
       (unless (and (= n1 n2) (= m1 m2))
         (error "array shape mismatch"))
       (let* ((C     (make-eigen-array 0 n1 0 m1))
              (data3 (slot-ref C 'backing-storage)))
         (,(symbol-append 'eigen-matrix- op1) data1 n1 m1 data2 n2 m2 data3)
         C))))
;; (行列2個の演算(破壊的変更版))
(define-macro (define-eigen-array-op! op1)
  `(define-method ,(symbol-append 'eigen-array- op1 '!) ((C <f64array>) (A <f64array>) (B <f64array>))
     (check-array-rank A B)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1))
           (data2 (slot-ref B 'backing-storage))
           (n2    (array-length B 0))
           (m2    (array-length B 1))
           (data3 (slot-ref C 'backing-storage))
           (n3    (array-length C 0))
           (m3    (array-length C 1)))
       (unless (and (= n1 n2 n3) (= m1 m2 m3))
         (error "array shape mismatch"))
       (,(symbol-append 'eigen-matrix- op1) data1 n1 m1 data2 n2 m2 data3)
       C)))
;; (行列とスカラーの演算)
(define-macro (define-eigen-array-op-scalar op1 op2)
  `(define-method ,(symbol-append 'eigen-array- op1) ((A <f64array>) (r <real>))
     (check-array-rank A)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1)))
       (let* ((B     (make-eigen-array 0 n1 0 m1))
              (data2 (slot-ref B 'backing-storage)))
         (,(symbol-append 'eigen-matrix- op2) data1 n1 m1 r data2)
         B))))
;; (行列とスカラーの演算(破壊的変更版))
(define-macro (define-eigen-array-op-scalar! op1 op2)
  `(define-method ,(symbol-append 'eigen-array- op1 '!) ((B <f64array>) (A <f64array>) (r <real>))
     (check-array-rank A)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1))
           (data2 (slot-ref B 'backing-storage))
           (n2    (array-length B 0))
           (m2    (array-length B 1)))
       (unless (and (= n1 n2) (= m1 m2))
         (error "array shape mismatch"))
       (,(symbol-append 'eigen-matrix- op2) data1 n1 m1 r data2)
       B)))
;; (行列1個の演算)
(define-macro (define-eigen-array-op-unary op1)
  `(define-method ,(symbol-append 'eigen-array- op1) ((A <f64array>))
     (check-array-rank A)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1)))
       (let* ((B     (make-eigen-array 0 n1 0 m1))
              (data2 (slot-ref B 'backing-storage)))
         (,(symbol-append 'eigen-matrix- op1) data1 n1 m1 data2)
         B))))
;; (行列1個の演算(破壊的変更版))
(define-macro (define-eigen-array-op-unary! op1)
  `(define-method ,(symbol-append 'eigen-array- op1 '!) ((B <f64array>) (A <f64array>))
     (check-array-rank A)
     (let ((data1 (slot-ref A 'backing-storage))
           (n1    (array-length A 0))
           (m1    (array-length A 1))
           (data2 (slot-ref B 'backing-storage))
           (n2    (array-length B 0))
           (m2    (array-length B 1)))
       (unless (and (= n1 n2) (= m1 m2))
         (error "array shape mismatch"))
       (,(symbol-append 'eigen-matrix- op1) data1 n1 m1 data2)
       B)))

;; 行列の和を計算
(define-eigen-array-op  add)
(define-eigen-array-op! add)

;; 行列とスカラーの和を計算
(define-eigen-array-op-scalar  add add-scalar)
(define-eigen-array-op-scalar! add add-scalar)

;; 行列の差を計算
(define-eigen-array-op  sub)
(define-eigen-array-op! sub)

;; 行列とスカラーの差を計算
(define-eigen-array-op-scalar  sub sub-scalar)
(define-eigen-array-op-scalar! sub sub-scalar)

;; 行列の積を計算
(define-method eigen-array-mul ((A <f64array>) (B <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (= m1 n2)
      (error "array shape mismatch"))
    (let* ((C     (make-eigen-array 0 n1 0 m2)) ; 結果は n1 x m2 になる
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-mul data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列の積を計算(破壊的変更版)
(define-method eigen-array-mul! ((C <f64array>) (A <f64array>) (B <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1))
        (data3 (slot-ref C 'backing-storage))
        (n3    (array-length C 0))
        (m3    (array-length C 1)))
    (unless (and (= m1 n2) (= n1 n3) (= m2 m3)) ; 結果は n1 x m2 になる
      (error "array shape mismatch"))
    (eigen-matrix-mul data1 n1 m1 data2 n2 m2 data3)
    C))

;; 行列の要素の積を計算
(define-eigen-array-op  mul-elements)
(define-eigen-array-op! mul-elements)

;; 行列とスカラーの積を計算
(define-eigen-array-op-scalar  mul-elements mul-scalar)
(define-eigen-array-op-scalar! mul-elements mul-scalar)

;; 行列とスカラーの割り算を計算
(define-eigen-array-op-scalar  div div-scalar)
(define-eigen-array-op-scalar! div div-scalar)

;; 行列の要素のべき乗を計算
(define-eigen-array-op-scalar  pow pow)
(define-eigen-array-op-scalar! pow pow)

;; 行列の要素を指数として、自然対数の底eのべき乗を計算
(define-eigen-array-op-unary  exp)
(define-eigen-array-op-unary! exp)

;; 行列の要素に対して、自然対数を計算
(define-eigen-array-op-unary  log)
(define-eigen-array-op-unary! log)

;; 行列の要素に対して、シグモイド関数を計算
(define-eigen-array-op-unary  sigmoid)
(define-eigen-array-op-unary! sigmoid)

;; 行列の要素に対して、ReLU関数を計算
(define-eigen-array-op-unary  relu)
(define-eigen-array-op-unary! relu)

;; 行列の要素に対して、ステップ関数を計算
(define-eigen-array-op-unary  step)
(define-eigen-array-op-unary! step)

;; 行列の要素の和を計算
(define-method eigen-array-sum ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (eigen-matrix-sum data1 n1 m1)))

;; 行列の要素の最小値を計算
(define-method eigen-array-min ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-min data1 n1 m1)))

;; 行列の要素の最大値を計算
(define-method eigen-array-max ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-max data1 n1 m1)))

;; 行列の要素の平均値を計算
(define-method eigen-array-mean ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-mean data1 n1 m1)))

;; 行列のトレースを計算
(define-method eigen-array-trace ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (eigen-matrix-trace data1 n1 m1)))

;; 行列式を計算
(define-method eigen-array-determinant ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (eigen-matrix-determinant data1 n1 m1)))

;; 転置行列を計算
(define-method eigen-array-transpose ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (make-eigen-array 0 m1 0 n1)) ; 結果は m1 x n1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-transpose data1 n1 m1 data2)
      B)))

;; 転置行列を計算(破壊的変更版)
(define-method eigen-array-transpose! ((B <f64array>) (A <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n2 m1) (= m2 n1))           ; 結果は m1 x n1 になる
      (error "array shape mismatch"))
    (eigen-matrix-transpose data1 n1 m1 data2)
    B))

;; 逆行列を計算
(define-method eigen-array-inverse ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (let* ((B     (make-eigen-array 0 m1 0 n1)) ; 結果は m1 x n1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-inverse data1 n1 m1 data2)
      B)))

;; 逆行列を計算(破壊的変更版)
(define-method eigen-array-inverse! ((B <f64array>) (A <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (unless (and (= n2 m1) (= m2 n1))           ; 結果は m1 x n1 になる
      (error "array shape mismatch"))
    (eigen-matrix-inverse data1 n1 m1 data2)
    B))

;; AX=B となる X を求める
(define-method eigen-array-solve ((A <f64array>) (B <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0) (> n2 0) (> m2 0))
      (error "invalid array shape"))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array A's shape must be square"))
    (unless (= n1 n2)
      (error "array shape mismatch"))
    (let* ((X     (make-eigen-array 0 m1 0 m2)) ; 結果は m1 x m2 になる
           (data3 (slot-ref X 'backing-storage)))
      (eigen-matrix-solve data1 n1 m1 data2 n2 m2 data3)
      X)))

;; AX=B となる X を求める(破壊的変更版)
(define-method eigen-array-solve! ((X <f64array>) (A <f64array>) (B <f64array>))
  (check-array-rank A B X)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1))
        (data3 (slot-ref X 'backing-storage))
        (n3    (array-length X 0))
        (m3    (array-length X 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0) (> n2 0) (> m2 0))
      (error "invalid array shape"))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array A's shape must be square"))
    (unless (= n1 n2)
      (error "array shape mismatch (A and B)"))
    (unless (and (= n3 m1) (= m3 m2))           ; 結果は m1 x m2 になる
      (error "array shape mismatch (X)"))
    (eigen-matrix-solve data1 n1 m1 data2 n2 m2 data3)
    X))

;; 行列から行を抜き出す
(define-method eigen-array-row ((A <f64array>) (i1r <integer>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0))))
    (unless (and (>= i1 0) (< i1 n1))
      (error "invalid index value"))
    (let* ((B     (make-eigen-array 0 1 0 m1))  ; 結果は 1 x m1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-row data1 n1 m1 data2 i1)
      B)))

;; 行列から行を抜き出す(破壊的変更版)
(define-method eigen-array-row! ((B <f64array>) (A <f64array>) (i1r <integer>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (>= i1 0) (< i1 n1))
      (error "invalid index value"))
    (unless (and (= n2 1) (= m2 m1))            ; 結果は 1 x m1 になる
      (error "array shape mismatch"))
    (eigen-matrix-row data1 n1 m1 data2 i1)
    B))

;; 行列から列を抜き出す
(define-method eigen-array-col ((A <f64array>) (j1r <integer>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (j1    (- j1r (array-start A 1))))
    (unless (and (>= j1 0) (< j1 m1))
      (error "invalid index value"))
    (let* ((B     (make-eigen-array 0 n1 0 1))  ; 結果は n1 x 1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-col data1 n1 m1 data2 j1)
      B)))

;; 行列から列を抜き出す(破壊的変更版)
(define-method eigen-array-col! ((B <f64array>) (A <f64array>) (j1r <integer>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (j1    (- j1r (array-start A 1)))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (>= j1 0) (< j1 m1))
      (error "invalid index value"))
    (unless (and (= n2 n1) (= m2 1))            ; 結果は n1 x 1 になる
      (error "array shape mismatch"))
    (eigen-matrix-col data1 n1 m1 data2 j1)
    B))

;; 行列から一部を抜き出す
(define-method eigen-array-block ((A <f64array>)
                                  (i1r <integer>) (j1r <integer>)
                                  (n2  <integer>) (m2  <integer>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (j1    (- j1r (array-start A 1))))
    (unless (and (>= n2 0) (>= m2 0))
      (error "invalid block size"))
    (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n2) n1) (<= (+ j1 m2) m1))
      (error "invalid block range"))
    (let* ((B     (make-eigen-array 0 n2 0 m2)) ; 結果は n2 x m2 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-block data1 n1 m1 data2 n2 m2 i1 j1)
      B)))

;; 行列から一部を抜き出す(破壊的変更版)
(define-method eigen-array-block! ((B <f64array>)
                                   (A <f64array>)
                                   (i1r <integer>) (j1r <integer>)
                                   (n2  <integer>) (m2  <integer>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (j1    (- j1r (array-start A 1)))
        (data2 (slot-ref B 'backing-storage))
        (n2a   (array-length B 0))
        (m2a   (array-length B 1)))
    (unless (and (>= n2 0) (>= m2 0))
      (error "invalid block size"))
    (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n2) n1) (<= (+ j1 m2) m1))
      (error "invalid block range"))
    (unless (and (= n2a n2) (= m2a m2))         ; 結果は n2 x m2 になる
      (error "array shape mismatch"))
    (eigen-matrix-block data1 n1 m1 data2 n2 m2 i1 j1)
    B))

;; 行列から一部を抜き出してコピー
(define-method eigen-array-block ((A <f64array>)
                                  (i1r <integer>) (j1r <integer>)
                                  (n3  <integer>) (m3  <integer>)
                                  (B <f64array>)
                                  (i2r <integer>) (j2r <integer>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (j1    (- j1r (array-start A 1)))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1))
        (i2    (- i2r (array-start B 0)))
        (j2    (- j2r (array-start B 1))))
    (unless (and (>= n3 0) (>= m3 0))
      (error "invalid block size"))
    (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n3) n1) (<= (+ j1 m3) m1))
      (error "invalid block range for copy-from"))
    (unless (and (>= i2 0) (>= j2 0) (<= (+ i2 n3) n2) (<= (+ j2 m3) m2))
      (error "invalid block range for copy-to"))
    (let* ((C     (make-eigen-array 0 n2 0 m2)) ; 結果は n2 x m2 になる
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-block-copy data1 n1 m1 data2 n2 m2 data3 n3 m3 i1 j1 i2 j2)
      C)))

;; 行列から一部を抜き出してコピー(破壊的変更版)
(define-method eigen-array-block! ((C <f64array>)
                                   (A <f64array>)
                                   (i1r <integer>) (j1r <integer>)
                                   (n3  <integer>) (m3  <integer>)
                                   (B <f64array>)
                                   (i2r <integer>) (j2r <integer>))
  (check-array-rank A B C)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (j1    (- j1r (array-start A 1)))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1))
        (i2    (- i2r (array-start B 0)))
        (j2    (- j2r (array-start B 1)))
        (data3 (slot-ref C 'backing-storage))
        (n3a   (array-length C 0))
        (m3a   (array-length C 1)))
    (unless (and (>= n3 0) (>= m3 0))
      (error "invalid block size"))
    (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n3) n1) (<= (+ j1 m3) m1))
      (error "invalid block range for copy-from"))
    (unless (and (>= i2 0) (>= j2 0) (<= (+ i2 n3) n2) (<= (+ j2 m3) m2))
      (error "invalid block range for copy-to"))
    (unless (and (= n3a n2) (= m3a m2))         ; 結果は n2 x m2 になる
      (error "array shape mismatch"))
    (eigen-matrix-block-copy data1 n1 m1 data2 n2 m2 data3 n3 m3 i1 j1 i2 j2)
    C))

