;; -*- coding: utf-8 -*-
;;
;; eigenmat.scm
;; 2019-2-28 v1.18
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
    use-eigen-array-cache
    eigen-make-array
    eigen-array
    eigen-array-nearly=?
    eigen-array-nearly-zero?
    eigen-array-add
    eigen-array-sub
    eigen-array-mul
    eigen-array-mul-elements
    eigen-array-div
    eigen-array-pow
    eigen-array-exp
    eigen-array-log
    eigen-array-sigmoid
    eigen-array-relu
    eigen-array-step
    eigen-array-sum
    eigen-array-min
    eigen-array-max
    eigen-array-mean
    eigen-array-trace
    eigen-array-determinant
    eigen-array-transpose
    eigen-array-inverse
    eigen-array-solve
    eigen-array-block
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

;; 行列のコピー(Gauche v0.9.7 以後には存在)
(define array-copy
  (if (global-variable-bound? 'gauche.array 'array-copy)
    (with-module gauche.array array-copy)
    (lambda (A)
      (make (class-of A)
        :start-vector (slot-ref A 'start-vector)
        :end-vector   (slot-ref A 'end-vector)
        :mapper       (slot-ref A 'mapper)
        :backing-storage (let1 v (slot-ref A 'backing-storage)
                           (if (vector? v)
                             (vector-copy v)
                             (uvector-copy v)))))))

;; 行列の情報取得(エラーチェックなし)
(define (array-rank   A)
  (s32vector-length (slot-ref A 'start-vector)))
(define (array-start  A dim)
  (s32vector-ref    (slot-ref A 'start-vector) dim))
(define (array-end    A dim)
  (s32vector-ref    (slot-ref A 'end-vector)   dim))
(define (array-length A dim)
  (- (s32vector-ref (slot-ref A 'end-vector)   dim)
     (s32vector-ref (slot-ref A 'start-vector) dim)))

;; 行列のチェック
(define (check-array . As)
  (every
   (lambda (A)
     ;; 次元数のチェック
     (unless (= (array-rank A) 2)
       (error "array rank must be 2"))
     ;; shapeのチェック
     (unless (and (>= (array-length A 0) 0)
                  (>= (array-length A 1) 0))
       (error "invalid array shape")))
   As))

;; == ここから 公開I/F ==

;; 行列のキャッシュ(ハッシュテーブル)
(define use-eigen-array-cache #t) ; 使用有無
(define array-cache-table (make-hash-table 'equal?))

;; 行列の生成
;; (キャッシュが存在すれば、それをコピーして返す
;;  (生成よりコピーの方が高速なため))
(define (eigen-make-array ns ne ms me)
  (if use-eigen-array-cache
    (let1 key (s32vector ns ne ms me)
      (if-let1 A (hash-table-get array-cache-table key #f)
        (array-copy A)
        (rlet1 B (make-f64array (shape ns ne ms me) 0)
          (hash-table-put! array-cache-table key B))))
    (make-f64array (shape ns ne ms me) 0)))

;; 行列の初期化データ付き生成
(define (eigen-array ns ne ms me . inits)
  (rlet1 A (eigen-make-array ns ne ms me)
    (f64vector-copy! (slot-ref A 'backing-storage)
                     0 (list->f64vector inits))))

;; 行列の一致チェック
(define-method eigen-array-nearly=? ((A <f64array>)
                                     (B <f64array>)
                                     :optional (precision 1e-12))
  (check-array A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "different array shapes"))
    (eigen-matrix-nearly-p data1 n1 m1 data2 n2 m2 precision)))

;; 行列のゼロチェック
(define-method eigen-array-nearly-zero? ((A <f64array>)
                                         :optional (precision 1e-12))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (eigen-matrix-nearly-zero-p data1 n1 m1 precision)))

;; 行列の和を計算
(define-method eigen-array-add ((A <f64array>) (B <f64array>))
  (check-array A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "can't add (array shapes mismatch)"))
    (let* ((C     (eigen-make-array 0 n1 0 m1))
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-add data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列とスカラーの和を計算
(define-method eigen-array-add ((A <f64array>) (r <real>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-add-scalar data1 n1 m1 r data2)
      B)))

;; 行列の差を計算
(define-method eigen-array-sub ((A <f64array>) (B <f64array>))
  (check-array A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "can't subtract (array shapes mismatch)"))
    (let* ((C     (eigen-make-array 0 n1 0 m1))
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-sub data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列とスカラーの差を計算
(define-method eigen-array-sub ((A <f64array>) (r <real>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-sub-scalar data1 n1 m1 r data2)
      B)))

;; 行列の積を計算
(define-method eigen-array-mul ((A <f64array>) (B <f64array>))
  (check-array A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (= m1 n2)
      (error "can't multiply (array shapes mismatch)"))
    (let* ((C     (eigen-make-array 0 n1 0 m2)) ; 結果は n1 x m2 になる
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-mul data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列の要素の積を計算
(define-method eigen-array-mul-elements ((A <f64array>) (B <f64array>))
  (check-array A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "can't multiply elements (array shapes mismatch)"))
    (let* ((C     (eigen-make-array 0 n1 0 m1))
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-mul-elements data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列とスカラーの積を計算
(define-method eigen-array-mul-elements ((A <f64array>) (r <real>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-mul-scalar data1 n1 m1 r data2)
      B)))

;; 行列とスカラーの割り算を計算
(define-method eigen-array-div ((A <f64array>) (r <real>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-div-scalar data1 n1 m1 r data2)
      B)))

;; 行列の要素のべき乗を計算
(define-method eigen-array-pow ((A <f64array>) (r <real>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-pow data1 n1 m1 r data2)
      B)))

;; 行列の要素を指数として、自然対数の底eのべき乗を計算
(define-method eigen-array-exp ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-exp data1 n1 m1 data2)
      B)))

;; 行列の要素に対して、自然対数を計算
(define-method eigen-array-log ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-log data1 n1 m1 data2)
      B)))

;; 行列の要素に対して、シグモイド関数を計算
(define-method eigen-array-sigmoid ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-sigmoid data1 n1 m1 data2)
      B)))

;; 行列の要素に対して、ReLU関数を計算
(define-method eigen-array-relu ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-relu data1 n1 m1 data2)
      B)))

;; 行列の要素に対して、ステップ関数を計算
(define-method eigen-array-step ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 n1 0 m1))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-step data1 n1 m1 data2)
      B)))

;; 行列の要素の和を計算
(define-method eigen-array-sum ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (eigen-matrix-sum data1 n1 m1)))

;; 行列の要素の最小値を計算
(define-method eigen-array-min ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-min data1 n1 m1)))

;; 行列の要素の最大値を計算
(define-method eigen-array-max ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-max data1 n1 m1)))

;; 行列の要素の平均値を計算
(define-method eigen-array-mean ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    (eigen-matrix-mean data1 n1 m1)))

;; 行列のトレースを計算
(define-method eigen-array-trace ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (eigen-matrix-trace data1 n1 m1)))

;; 行列式を計算
(define-method eigen-array-determinant ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (eigen-matrix-determinant data1 n1 m1)))

;; 転置行列を計算
(define-method eigen-array-transpose ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (let* ((B     (eigen-make-array 0 m1 0 n1)) ; 結果は m1 x n1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-transpose data1 n1 m1 data2)
      B)))

;; 逆行列を計算
(define-method eigen-array-inverse ((A <f64array>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    ;; (0を許可すると実行時エラーになる)
    (unless (and (> n1 0) (> m1 0))
      (error "invalid array shape"))
    ;; (正方行列でなくても何かしら計算する?)
    ;(unless (= n1 m1)
    ;  (error "array shape must be square"))
    (let* ((B     (eigen-make-array 0 m1 0 n1)) ; 結果は m1 x n1 になる
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-inverse data1 n1 m1 data2)
      B)))

;; AX=B となる X を求める
(define-method eigen-array-solve ((A <f64array>) (B <f64array>))
  (check-array A B)
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
      (error "can't solve (array shapes mismatch)"))
    (let* ((X     (eigen-make-array 0 m1 0 m2)) ; 結果は m1 x m2 になる
           (data3 (slot-ref X 'backing-storage)))
      (eigen-matrix-solve data1 n1 m1 data2 n2 m2 data3)
      X)))

;; 行列から一部を抜き出す
(define-method eigen-array-block ((A <f64array>)
                                  (i1r <integer>) (j1r <integer>)
                                  (n2 <integer>) (m2 <integer>))
  (check-array A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (i1    (- i1r (array-start A 0)))
        (j1    (- j1r (array-start A 1))))
    (unless (and (>= n2 0) (>= m2 0))
      (error "invalid block size"))
    (unless (and (>= i1 0) (>= j1 0) (<= (+ i1 n2) n1) (<= (+ j1 m2) m1))
      (error "invalid block range"))
    (let* ((B     (eigen-make-array 0 n2 0 m2))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-block data1 n1 m1 data2 n2 m2 i1 j1)
      B)))

;; 行列から一部を抜き出してコピー
(define-method eigen-array-block ((A <f64array>)
                                  (i1r <integer>) (j1r <integer>)
                                  (n3 <integer>) (m3 <integer>)
                                  (B <f64array>)
                                  (i2r <integer>) (j2r <integer>))
  (check-array A B)
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
    (let* ((C     (eigen-make-array 0 n2 0 m2))
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-block-copy data1 n1 m1 data2 n2 m2 data3 n3 m3 i1 j1 i2 j2)
      C)))

