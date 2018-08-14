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

extern int eigen_matrix_nearly_p_sub(double* data1, int n1, int m1,
                                     double* data2, int n2, int m2,
                                     double abs_tol);

extern int eigen_matrix_mul_sub(double* data1, int n1, int m1,
                                double* data2, int n2, int m2,
                                double* data3);

extern double eigen_matrix_determinant_sub(double* data1, int n1, int m1);

extern int eigen_matrix_inverse_sub(double* data1, int n1, int m1,
                                    double* data2);

extern int eigen_matrix_solve_sub(double* data1, int n1, int m1,
                                  double* data2, int n2, int m2,
                                  double* data3);

/* need for C++ */
extern void Scm_Init_eigenmat();

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_EIGENMAT_H */
