#include <R.h>
#include <Rmath.h>
#include <omp.h>
#include <gsl/gsl_cdf.h>

// Inverse Beta Logit Transformation
void beta_logit_tran_inverse_C(double *z, double *result, int *len, double *a, double *b, double *b1, double *b2) {
#pragma omp parallel for

  for (int i = 0; i < *len; i++) {

    double exp_z = exp(z[i]);

    // # Reverse logit transformation
    double logit_inv = exp_z / (1 + exp_z);

    // Find beta quantile
    double beta_quantile = gsl_cdf_beta_Pinv(logit_inv, *b1, *b2);

    // Scale back to the original domain
    result[i] = *a + beta_quantile * (*b - *a);

    // Replace NA values with b
    if (ISNAN(result[i])) {
      result[i] = *b;
    }
  }
}
