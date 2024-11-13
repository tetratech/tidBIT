#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the function prototypes
void Derr_recursive_C(int *dBlks, double *rhos, int *dimension, int *total_size, double *ie, double *de);
SEXP mat_mult(SEXP matA, SEXP matB, SEXP nrowA, SEXP ncolA, SEXP ncolB);
void beta_logit_tran_inverse_C(double *z, double *result, int *len, double *a, double *b, double *b1, double *b2);

// Register the C routines
static const R_CMethodDef CEntries[] = {
  {"Derr_recursive_C", (DL_FUNC) &Derr_recursive_C, 6},
  {"beta_logit_tran_inverse_C", (DL_FUNC) &beta_logit_tran_inverse_C, 7},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
  {"mat_mult", (DL_FUNC) &mat_mult, 5},
  {NULL, NULL, 0}
};

// Initialization function
void R_init_tidBIT(DllInfo *dll) {
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
