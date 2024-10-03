
# fun_$_cholesky_decomposition.R

#' Wrapper for Multi-Dimensional AR Error Generation
#'
#' This function generates multi-dimensional autoregressive (AR) errors using a recursive C function.
#' It provides a wrapper around the C-based error generation function for efficient computation,
#' handling multi-dimensional data.
#'
#' @param dBlks A list of block sizes for each dimension. Each element of the list represents the block size
#'              for the corresponding dimension (e.g., `list(d1Blk, d2Blk, d3Blk)`).
#' @param rhos A list of correlation coefficients for each dimension. The first element is the AR(1)
#'             coefficient (phi), while subsequent elements represent correlations across higher dimensions.
#' @param dimension An integer representing the number of dimensions to process. If `NULL`, it is
#'                  automatically determined from the length of `dBlks`.
#'
#' @details
#' This function uses pre-generated random numbers from R, which are passed to the recursive
#' C function (`Derr_recursive_C`) for multi-dimensional AR error generation. The function can handle
#' any number of dimensions and is optimized for speed.
#'
#' @return A numeric vector representing the multi-dimensional AR errors.
#'
#' @export
#'
#' @examples
#' # Example usage
#' dBlks <- list(10, 5, 3)
#' rhos <- list(0.9, 0.8, 0.7)
#' result <- generate_Cholesky_decomp_C(dBlks, rhos)
generate_Cholesky_decomp_C <- function(dBlks, rhos, dimension = NULL) {
  # Check that dBlks and rhos are provided and are lists
  if (is.null(dBlks) || !is.list(dBlks)) {
    stop("dBlks must be a list of block sizes.")
  }

  if (is.null(rhos) || !is.list(rhos)) {
    stop("rhos must be a list of correlation coefficients.")
  }

  # Automatically determine the dimension if not provided
  if (is.null(dimension)) {
    dimension <- length(dBlks)
  }

  if (length(dBlks) < dimension) {
    stop("dBlks must have at least 'dimension' elements.")
  }

  if (length(rhos) < dimension) {
    stop("rhos must have at least 'dimension' elements.")
  }

  # Convert lists to vectors
  dBlks_vec <- as.integer(unlist(dBlks))
  rhos_vec <- as.double(unlist(rhos))
  total_size <- prod(dBlks_vec[1:dimension])

  # Generate all random numbers at once in R
  ie <- rnorm(total_size, 0, 1)

  # Initialize the output vector for dependent errors (de)
  de <- numeric(total_size)

  # Call the C function for recursive error generation
  de <- .C("Derr_recursive_C", dBlks_vec, rhos_vec, as.integer(dimension), as.integer(total_size), as.double(ie), de)

  de <- de[[6]]

  rm(ie)

  return(de)
}
