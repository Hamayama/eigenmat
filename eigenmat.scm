;; -*- coding: utf-8 -*-
;;
;; eigenmat.scm
;; 2018-8-14 v1.02
;;
;; ＜内容＞
;;   Gauche で、Eigen ライブラリ を使って行列の高速演算を行うためのモジュールです。
;;   Eigen は、C++ で書かれた線形代数用のライブラリです ( http://eigen.tuxfamily.org )。
;;   現状、本モジュールは、標準の gauhce.array モジュールにおける
;;   <f64array> クラスのごく一部の演算にのみ対応しています。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/eigenmat
;;
(define-module eigenmat
  (use gauche.uvector)
  (use gauche.array)
  (export
    test-eigenmat
    eigen-array-nearly=?
    eigen-array-mul
    eigen-array-determinant
    eigen-array-inverse
    eigen-array-solve))
(select-module eigenmat)

;; Loads extension
(dynamic-load "eigenmat")

;;
;; Put your Scheme definitions here
;;

;; 行列の次元数のチェック(内部処理用)
(define (check-array-rank . As)
  (unless (every (lambda (A) (= (array-rank A) 2)) As)
    (error "array rank must be 2")))

;; 行列の一致チェック
(define-method eigen-array-nearly=? ((A <f64array>) (B <f64array>) :optional (abs-tol 1e-4))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (and (= n1 n2) (= m1 m2))
      (error "different array shapes"))
    (eigen-matrix-nearly data1 n1 m1 data2 n2 m2 abs-tol)))

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
      (error "can't mul (array shapes mismatch)"))
    (let* ((C     (make-f64array (shape 0 n1 0 m2) 0))
           (data3 (slot-ref C 'backing-storage)))
      (eigen-matrix-mul data1 n1 m1 data2 n2 m2 data3)
      C)))

;; 行列式を計算
(define-method eigen-array-determinant ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (unless (= n1 m1)
      (error "array shape must be square"))
    (eigen-matrix-determinant data1 n1 m1)))

;; 逆行列を計算
(define-method eigen-array-inverse ((A <f64array>))
  (check-array-rank A)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1)))
    (unless (= n1 m1)
      (error "array shape must be square"))
    (let* ((B     (make-f64array (shape 0 n1 0 m1) 0))
           (data2 (slot-ref B 'backing-storage)))
      (eigen-matrix-inverse data1 n1 m1 data2)
      B)))

;; AX=B となる X を求める
(define-method eigen-array-solve ((A <f64array>) (B <f64array>))
  (check-array-rank A B)
  (let ((data1 (slot-ref A 'backing-storage))
        (n1    (array-length A 0))
        (m1    (array-length A 1))
        (data2 (slot-ref B 'backing-storage))
        (n2    (array-length B 0))
        (m2    (array-length B 1)))
    (unless (= n1 m1)
      (error "array A's shape must be square"))
    (unless (= m1 n2)
      (error "can't solve (array shapes mismatch)"))
    (let* ((X     (make-f64array (shape 0 n1 0 m2) 0))
           (data3 (slot-ref X 'backing-storage)))
      (eigen-matrix-solve data1 n1 m1 data2 n2 m2 data3)
      X)))

