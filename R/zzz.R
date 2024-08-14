

# zzz.R

.onAttach <- function(libname, pkgname) {
  suppressPackageStartupMessages(library(GGally, quietly = TRUE, warn.conflicts = FALSE))
}
