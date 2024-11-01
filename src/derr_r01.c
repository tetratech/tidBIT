#include <R.h>
#include <Rmath.h>
#include <omp.h>  

// C function to generate 1D AR(1) errors with pre-generated random numbers
void D1err_C(int *n, double *rho, double *ie, double *de) {
  
  // Calculate c_rho
  double c_rho = sqrt(1 - (*rho) * (*rho));  
  
  // 2024.10.03 choice: Exclude this step for consistency across all dimensions
  // // Scale the first random number by c_rho
  // de[0] = ie[0] * c_rho;  
  de[0] = ie[0];  // instead just initialize first element
  
  // Apply the autoregressive process  
  for (int i = 1; i < *n; i++) {
    de[i] = (*rho) * de[i - 1] + c_rho * ie[i];  // Recursive AR(1) relation
  }
  
}


// Recursive C function for multi-dimensional AR errors
void Derr_recursive_C(int *dBlks, double *rhos, int *dimension, int *total_size, double *ie, double *de) {
  
  // Base case: 1D AR(1) errors using pre-generated random numbers
  if (*dimension == 1) {
    D1err_C(&dBlks[0], &rhos[0], ie, de);
    return;
  }
  
  int d_current = dBlks[*dimension - 1];
  int d_previous_total = *total_size / d_current;  // Calculate size of previous dimensions
  
  // Recursively populate the columns with (dimension-1)-dimensional errors
  for (int i = 0; i < d_current; i++) {
    Derr_recursive_C(dBlks, rhos, &(int){*dimension - 1}, &d_previous_total, ie + i * d_previous_total, de + i * d_previous_total);
  }
  
  // Calculate c_rho
  double rho = rhos[*dimension - 1];
  double c_rho = sqrt(1 - rho * rho);
  
  // 2024.10.03 choice: Exclude this step for consistency across all dimensions
  // // Scale the first column (equivalent to bdem[, 1] <- bdem[, 1] * c.rho in R)
  // for (int j = 0; j < d_previous_total; j++) {
  //   de[j] = de[j] * c_rho;
  // }
  
  // Apply AR(1)-like dependence across the blocks (between columns)
  for (int i = 1; i < d_current; i++) {
    for (int j = 0; j < d_previous_total; j++) {
      de[j + i * d_previous_total] = rho * de[j + (i - 1) * d_previous_total] + c_rho * de[j + i * d_previous_total];
    }
  }
}

// Recursive C function for multi-dimensional AR errors with multi-processing (omp)
void Derr_recursive_Cmp(int *dBlks, double *rhos, int *dimension, int *total_size, double *ie, double *de) {
  
  // Base case: 1D AR(1) errors using pre-generated random numbers
  if (*dimension == 1) {
    D1err_C(&dBlks[0], &rhos[0], ie, de);
    return;
  }
  
  int d_current = dBlks[*dimension - 1];
  int d_previous_total = *total_size / d_current;  // Calculate size of previous dimensions
  
  // Recursively populate the columns with (dimension-1)-dimensional errors
#pragma omp parallel for
  for (int i = 0; i < d_current; i++) {
    Derr_recursive_Cmp(dBlks, rhos, &(int){*dimension - 1}, &d_previous_total, ie + i * d_previous_total, de + i * d_previous_total);
  }
  
  // Calculate c_rho
  double rho = rhos[*dimension - 1];
  double c_rho = sqrt(1 - rho * rho);
  
  // 2024.10.03 choice: Exclude this step for consistency across all dimensions
  // // Scale the first column (equivalent to bdem[, 1] <- bdem[, 1] * c.rho in R)
  // #pragma omp parallel for
  //   for (int j = 0; j < d_previous_total; j++) {
  //     de[j] = de[j] * c_rho;
  //   }
  
  // Apply AR(1)-like dependence across the blocks (between columns)
  for (int i = 1; i < d_current; i++) {
#pragma omp parallel for
    for (int j = 0; j < d_previous_total; j++) {
      de[j + i * d_previous_total] = rho * de[j + (i - 1) * d_previous_total] + c_rho * de[j + i * d_previous_total];
    }
  }
}

