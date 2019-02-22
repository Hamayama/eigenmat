/*
 * eigenmat.h
 */

/* Prologue */
#ifndef GAUCHE_EIGENMAT_H
#define GAUCHE_EIGENMAT_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

/*
 * The following entry is a dummy one.
 * Replace it for your declarations.
 */

extern ScmObj test_eigenmat(void);

extern int eigen_matrix_nearly_p(double* data1, int n1, int m1,
                                 double* data2, int n2, int m2,
                                 double precision);

extern int eigen_matrix_nearly_zero_p(double* data1, int n1, int m1,
                                      double precision);

extern int eigen_matrix_add(double* data1, int n1, int m1,
                            double* data2, int n2, int m2,
                            double* data3);

extern int eigen_matrix_add_scalar(double* data1, int n1, int m1,
                                   double scalar, double* data2);

extern int eigen_matrix_sub(double* data1, int n1, int m1,
                            double* data2, int n2, int m2,
                            double* data3);

extern int eigen_matrix_sub_scalar(double* data1, int n1, int m1,
                                   double scalar, double* data2);

extern int eigen_matrix_mul(double* data1, int n1, int m1,
                            double* data2, int n2, int m2,
                            double* data3);

extern int eigen_matrix_mul_scalar(double* data1, int n1, int m1,
                                   double scalar, double* data2);

extern double eigen_matrix_determinant(double* data1, int n1, int m1);

extern int eigen_matrix_inverse(double* data1, int n1, int m1,
                                double* data2);

extern int eigen_matrix_solve(double* data1, int n1, int m1,
                              double* data2, int n2, int m2,
                              double* data3);

extern int eigen_matrix_block(double* data1, int n1, int m1,
                              double* data2, int i2, int j2, int n2, int m2);

/* need for C++ */
extern void Scm_Init_eigenmat();

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_EIGENMAT_H */
