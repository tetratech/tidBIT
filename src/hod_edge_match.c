#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <omp.h>

void hod_edge_match(
    int *group_rows,   // Grouped data (i, j, k, first_id, last_id) flattened into 2D row-major format
    int *num_groups,   // Number of groups
    double *real_y,    // real_y matrix (in place)
    int *nrow,         // Number of rows in real_y
    int *ncol,         // Number of columns in real_y
    int *debug         // Debug flag (1 = enable, 0 = disable)
) {


#pragma omp parallel for
  for (int g = 0; g < *num_groups; g++) {
    int idx = g * 5;  // Each group has 5 entries: i, j, k, first_id, last_id
    int i = group_rows[idx];
    int j = group_rows[idx + 1];
    int k = group_rows[idx + 2];
    int first_id = group_rows[idx + 3] - 1;  // Convert to 0-based index
    int last_id = group_rows[idx + 4] - 1;   // Convert to 0-based index

    if (first_id < 0 || last_id >= *nrow) {
      Rprintf("Error: Group (i=%d, j=%d, k=%d) indices out of bounds: [%d, %d]\n", i, j, k, first_id + 1, last_id + 1);
      continue;
    }

    if (*debug) {
      Rprintf("Group: i=%d, j=%d, k=%d, first_id=%d, last_id=%d\n", i, j, k, first_id, last_id);
    }

    // Skip groups where first_id + 1 > last_id
    if (first_id + 1 > last_id) {
      Rprintf("Warning: Skipping group (i=%d, j=%d, k=%d) with invalid range [%d, %d]\n",
              i, j, k, first_id + 1, last_id + 1);
      continue;
    }

    // Update real_y for this group
    for (int row = first_id + 1; row <= last_id; row++) {

      if (*debug) {
        Rprintf("Loop 1a: row=%d\n", row);
        Rprintf("Loop 1b: idx1=%d  idx2=%d\n", row + 0 * (*nrow), (row - 1) + (*ncol - 1) * (*nrow));
      }

      // Compute delta based on the first column of the current row and last column of the previous row
      double delta = (real_y[row + 0 * (*nrow)] - real_y[(row - 1) + (*ncol - 1) * (*nrow)]) / (*ncol);

      if (*debug) {
        Rprintf("Loop 1: delta=%f\n", delta);
      }

      for (int col = 0; col < *ncol; col++) {
        real_y[row + col * (*nrow)] += delta * (col + 1 - (*ncol / 2));
        if (*debug) {
          Rprintf("Loop 2: col=%d idx=%d, updated_val=%f\n", col, row + col * (*nrow), real_y[row + col * (*nrow)]);
        }
      }
    }


  }

  // print the real_y matrix in R-compatible format
  if (*debug) {
    Rprintf("real_y\n");
    for (int i = 0; i < *nrow; i++) {
      Rprintf("[");
      for (int j = 0; j < *ncol; j++) {
        Rprintf("%5.1f", real_y[i + j * (*nrow)]);  // Print element with padding
        if (j < *ncol - 1) Rprintf(" ");           // Add space between elements
      }
      Rprintf(" ]\n");  // End of row
    }
  }

}
