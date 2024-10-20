#include <R.h>
#include <Rinternals.h>
#include <omp.h>

// Function to perform matrix-matrix multiplication (A * B = C)
// where A is an N x M matrix and B is an M x P matrix (or vector when P = 1)
SEXP mat_mult(SEXP matA, SEXP matB, SEXP nrowA, SEXP ncolA, SEXP ncolB) {
  // Extract pointers to R data
  double *A = REAL(matA);  // Matrix A pointer
  double *B = REAL(matB);  // Matrix B pointer
  int N = INTEGER(nrowA)[0];  // Number of rows in A (and C)
  int M = INTEGER(ncolA)[0];  // Number of columns in A, rows in B
  int P = INTEGER(ncolB)[0];  // Number of columns in B (or 1 for vector multiplication)

  // Allocate memory for the result matrix C (N x P)
  SEXP C = PROTECT(allocMatrix(REALSXP, N, P));
  double *C_data = REAL(C);

  // Perform matrix multiplication in parallel: A (N x M) * B (M x P) = C (N x P)
#pragma omp parallel for collapse(2)
  for (int i = 0; i < N; i++) {  // Loop over rows of A
    for (int p = 0; p < P; p++) {  // Loop over columns of B
      double sum = 0.0;
      for (int j = 0; j < M; j++) {  // Loop over shared dimension
        sum += A[i + j * N] * B[j + p * M];  // Matrix A is in column-major, so is B
      }
      C_data[i + p * N] = sum;  // Store the result in C
    }
  }

  // Return the result matrix C
  UNPROTECT(1);
  return C;
}
