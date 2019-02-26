/*
 * eigenmat.c
 */

#define EIGEN_DEFAULT_TO_ROW_MAJOR 1 // データの格納順を行優先にする

#include "eigenmat.h"
#include <iostream>
#include <Eigen/Dense>

using namespace std;
using namespace Eigen;

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_eigenmat(void)
{
    MatrixXd m(2,2);
    m(0,0) = 3;
    m(1,0) = 2.5;
    m(0,1) = -1;
    m(1,1) = m(1,0) + m(0,1);
    //cerr << endl << m << endl;
    return SCM_MAKE_STR("eigenmat is working");
}

// 行列の一致チェック
int eigen_matrix_nearly_p(double* data1, int n1, int m1,
                          double* data2, int n2, int m2,
                          double precision) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (n1 != n2 || m1 != m2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    return A.isApprox(B, precision) ? TRUE : FALSE;
}

// 行列のゼロチェック
int eigen_matrix_nearly_zero_p(double* data1, int n1, int m1,
                               double precision) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return A.isMuchSmallerThan(precision) ? TRUE : FALSE;
}

// 行列の和を計算
int eigen_matrix_add(double* data1, int n1, int m1,
                     double* data2, int n2, int m2,
                     double* data3) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (n1 != n2 || m1 != m2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd C = A + B;
    Map<MatrixXd>(data3, n1, m1) = C;
    return TRUE;
}

// 行列とスカラーの和を計算
int eigen_matrix_add_scalar(double* data1, int n1, int m1,
                            double r, double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array() + r;
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の差を計算
int eigen_matrix_sub(double* data1, int n1, int m1,
                     double* data2, int n2, int m2,
                     double* data3) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (n1 != n2 || m1 != m2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd C = A - B;
    Map<MatrixXd>(data3, n1, m1) = C;
    return TRUE;
}

// 行列とスカラーの差を計算
int eigen_matrix_sub_scalar(double* data1, int n1, int m1,
                            double r, double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array() - r;
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の積を計算
int eigen_matrix_mul(double* data1, int n1, int m1,
                     double* data2, int n2, int m2,
                     double* data3) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (m1 != n2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd C = A * B;
    Map<MatrixXd>(data3, n1, m2) = C; // 第3引数はm1ではないので注意
    return TRUE;
}

// 行列の要素の積を計算
int eigen_matrix_mul_elements(double* data1, int n1, int m1,
                              double* data2, int n2, int m2,
                              double* data3) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (n1 != n2 || m1 != m2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd C = A.array() * B.array();
    Map<MatrixXd>(data3, n1, m1) = C;
    return TRUE;
}

// 行列とスカラーの積を計算
int eigen_matrix_mul_scalar(double* data1, int n1, int m1,
                            double r, double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array() * r;
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列とスカラーの割り算を計算
int eigen_matrix_div_scalar(double* data1, int n1, int m1,
                            double r, double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array() / r;
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の要素のべき乗を計算
int eigen_matrix_pow(double* data1, int n1, int m1,
                     double r, double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array().pow(r);
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の要素を指数として、自然対数の底eのべき乗を計算
int eigen_matrix_exp(double* data1, int n1, int m1,
                     double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array().exp();
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の要素の自然対数を計算
int eigen_matrix_log(double* data1, int n1, int m1,
                     double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.array().log();
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の要素に対するシグモイド関数を計算
static double sigmoid(double x) { return 1.0 / (1.0 + exp(-x)); }
int eigen_matrix_sigmoid(double* data1, int n1, int m1,
                         double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.unaryExpr(&sigmoid);
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// 行列の要素の和を計算
double eigen_matrix_sum(double* data1, int n1, int m1) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.sum();
}

// 行列の要素の最小値を計算
double eigen_matrix_min(double* data1, int n1, int m1) {
    // (0を許可すると実行時エラーになる)
    //if (n1 < 0 || m1 < 0) return FALSE;
    if (n1 <= 0 || m1 <= 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.minCoeff();
}

// 行列の要素の最大値を計算
double eigen_matrix_max(double* data1, int n1, int m1) {
    // (0を許可すると実行時エラーになる)
    //if (n1 < 0 || m1 < 0) return FALSE;
    if (n1 <= 0 || m1 <= 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.maxCoeff();
}

// 行列の要素の平均を計算
double eigen_matrix_mean(double* data1, int n1, int m1) {
    // (0を許可すると実行時エラーになる)
    //if (n1 < 0 || m1 < 0) return FALSE;
    if (n1 <= 0 || m1 <= 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.mean();
}

// 行列のトレースを計算
double eigen_matrix_trace(double* data1, int n1, int m1) {
    if (n1 < 0 || m1 < 0) return FALSE;
    // (正方行列でなくても何かしら計算する?)
    //if (n1 != m1) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.trace();
}

// 行列式を計算
double eigen_matrix_determinant(double* data1, int n1, int m1) {
    if (n1 < 0 || m1 < 0) return FALSE;
    // (正方行列でなくても何かしら計算する?)
    //if (n1 != m1) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    return (double)A.determinant();
}

// 転置行列を計算
int eigen_matrix_transpose(double* data1, int n1, int m1,
                           double* data2) {
    if (n1 < 0 || m1 < 0) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.transpose();
    Map<MatrixXd>(data2, m1, n1) = B; // m1, n1 の順なので注意
    return TRUE;
}

// 逆行列を計算
int eigen_matrix_inverse(double* data1, int n1, int m1,
                         double* data2) {
    // (0を許可すると実行時エラーになる)
    //if (n1 < 0 || m1 < 0) return FALSE;
    if (n1 <= 0 || m1 <= 0) return FALSE;
    // (正方行列でなくても何かしら計算する?)
    //if (n1 != m1) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.inverse();
    Map<MatrixXd>(data2, n1, m1) = B;
    return TRUE;
}

// AX=B となる X を求める
int eigen_matrix_solve(double* data1, int n1, int m1,
                       double* data2, int n2, int m2,
                       double* data3) {
    // (0を許可すると実行時エラーになる)
    //if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (n1 <= 0 || m1 <= 0 || n2 <= 0 || m2 <= 0) return FALSE;
    // (正方行列でなくても何かしら計算する?)
    //if (n1 != m1) return FALSE;
    if (m1 != n2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd X = A.partialPivLu().solve(B);
    Map<MatrixXd>(data3, n1, m2) = X; // m1 ではなく m2 なので注意
    return TRUE;
}

// 行列から一部を抜き出す
int eigen_matrix_block(double* data1, int n1, int m1,
                       double* data2, int n2, int m2,
                       int i1, int j1) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0) return FALSE;
    if (i1 < 0 || j1 < 0 || i1 + n2 > n1 || j1 + m2 > m1) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = A.block(i1, j1, n2, m2);
    Map<MatrixXd>(data2, n2, m2) = B;
    return TRUE;
}

// 行列から一部を抜き出してコピー
int eigen_matrix_block_copy(double* data1, int n1, int m1,
                            double* data2, int n2, int m2,
                            double* data3, int n3, int m3,
                            int i1, int j1, int i2, int j2) {
    if (n1 < 0 || m1 < 0 || n2 < 0 || m2 < 0 || n3 < 0 || m3 < 0) return FALSE;
    if (i1 < 0 || j1 < 0 || i1 + n3 > n1 || j1 + m3 > m1) return FALSE;
    if (i2 < 0 || j2 < 0 || i2 + n3 > n2 || j2 + m3 > m2) return FALSE;
    MatrixXd A = Map<MatrixXd>(data1, n1, m1);
    MatrixXd B = Map<MatrixXd>(data2, n2, m2);
    MatrixXd C = B;
    C.block(i2, j2, n3, m3) = A.block(i1, j1, n3, m3);
    Map<MatrixXd>(data3, n2, m2) = C;
    return TRUE;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_eigenmatlib(ScmModule*);

void Scm_Init_eigenmat(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(eigenmat);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("eigenmat", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_eigenmatlib(mod);
}
