#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the function prototypes
void Derr_recursive_C(int *dBlks, double *rhos, int *dimension, int *total_size, double *ie, double *de);

// Register the C routines
static const R_CMethodDef CEntries[] = {
  {"Derr_recursive_C", (DL_FUNC) &Derr_recursive_C, 6},
  {NULL, NULL, 0}
};

// Initialization function
void R_init_tidBIT(DllInfo *dll) {
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
