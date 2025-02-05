

#' Edge Matching for Higher-Order Dimensions
#'
#' This function is a wrapper around a C-based implementation for edge matching
#' across higher-order dimensions. It adjusts values in the `real_y` matrix based
#' on grouped summaries derived from the `int_grid` object.
#'
#' @param int_grid A data frame or tibble containing the grid structure. Must
#' include the columns `i`, `j`, `k`, `water_mask`, and `water_mask_id`.
#' @param real_y A numeric matrix where adjustments are applied. The dimensions
#' of this matrix are flexible and depend on the specific use case.
#'
#' @return The `real_y` matrix, updated in place with adjustments based on the
#' edge matching algorithm.
#'
#' @details
#' This function preprocesses the `int_grid` to identify contiguous blocks of
#' rows with valid water masks (where `water_mask == 1`). For each group defined
#' by unique combinations of `i`, `j`, and `k`, it computes the range of indices
#' (`water_mask_id_first` to `water_mask_id_last`) and applies edge adjustments
#' to the `real_y` matrix.
#'
#' The edge matching operation is implemented in C using OpenMP for parallelism.
#' Warnings are issued for groups where no valid range exists, and these groups
#' are skipped.
#'
#' @examples
#' # Example int_grid and real_y
#' int_grid <- tibble::tibble(
#'   i = c(1, 1, 1, 2, 2, 2),
#'   j = c(1, 1, 1, 1, 1, 1),
#'   k = c(1, 1, 1, 1, 1, 1),
#'   row_id = 1:6
#' )
#' real_y <- matrix(1:36, nrow = 6, ncol = 6, byrow=TRUE)
#'
#' # Perform edge matching
#' updated_real_y <- hod_edge_match_C(int_grid, real_y)
#'
#' @useDynLib tidBIT, .registration = TRUE
#' @export
hod_edge_match_C <- function(int_grid, real_y, debug = 0) {
  # Preprocess int_grid to extract grouped summaries
  int_grid_lst <- int_grid %>%
    # dplyr::filter(water_mask == 1) %>%
    dplyr::group_by(i, j, k) %>%
    dplyr::summarise(
      row_id_first = min(row_id),
      row_id_last = max(row_id),
      .groups = "drop"
    )

  # Flatten grouped summaries into a single integer vector
  group_rows <- int_grid_lst %>%
    dplyr::select(i, j, k, row_id_first, row_id_last) %>%
    as.matrix() %>%
    t() %>%
    as.integer()

  # Call the C function
  result <- .C(
    "hod_edge_match",
    group_rows = group_rows,
    num_groups = as.integer(nrow(int_grid_lst)),
    real_y = as.double(real_y),  # Pass real_y by reference for in-place update
    nrow = as.integer(nrow(real_y)),
    ncol = as.integer(ncol(real_y)),
    debug = as.integer(debug)
  )

  updated_real_y <- matrix(as.numeric(result[["real_y"]]), nrow = nrow(real_y), ncol = ncol(real_y))
  # print(updated_real_y)

  # Return updated real_y in matrix form
  return(updated_real_y)
}

