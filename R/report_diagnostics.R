# EWL, 20240208
# create report
#
# maybe have options to run "all" or "select" files.
# also an option to run a single report
# maybe create a function with options
#
# NOTE
# If add new objects need to add them to the RMDfile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @title CB4D Diagnostics Report
#'
#' @description Create diagnostics report for CB4D model output
#'
#' @details
#'
#' Default output is an HTML report in temporary directory.  But a Word document
#' (docx) is also an option.
#'
#' To switch between volume 1 and 2 reports change the parameters "path_rmd" and
#' "report_volume" to the correct report template and volume number.
#'
#' The default report templates are contained in the package but user versions
#' can be used as well.
#'
#' The report objects are created by the internal function `report_objects` and
#' are derived from the 4D model output files (in RDA format).  As long as the
#' objects are in the "env_cb4d" environment they can be used in the report.
#'
#' Users can access the temp directory, in Windows, from the R console via
#' `shell.exec(tempdir())`.
#'
#' @param fn_model_rda CB4D model output RDA files, single value or vector of
#' filenames.
#' @param dn_model_rda Directory containing all of the model RDA files.  If
#' files are in separate folders provide a vector of the same length as
#' fn_model_rda
#' @param format_rmd Notebook (RMD) output format, 'html' or 'docx'.
#' Default = "html"
#' @param path_rmd Path (directory and file name) of Notebook (RMD) file.
#' @param report_volume Report version (volume); 1 or 2.  Default = NULL
#' @param dn_results Directory to save created notebook(s). The directory will
#' be created if it does not exist.  Default is tempdir()
#' @param fn_report_prefix Prefix for generated Notebook output.  The base name
#' of the file will be the base name of the input RDA file.
#' Default = "cb4d_diagnostics~"
#' @param quiet Should printing of messages be suppressed.  This includes this
#' function and during rendering of the document (rmarkdown::render).
#' Default = TRUE
#'
#' @examples
#'
#' # Example 1, Select File(s), file name
#' fn_model_rda     <- "do~CB3~datahub~2016-2018~2023_10_07_005~by_term2~none.rda"
#' dn_model_rda     <- system.file("extdata", package = "tidBITcore")
#'
#' # Other input parameters
#' format_rmd       <- "html"
#' path_rmd         <- file.path(system.file("rmd", package = "tidBITcore"),
#'                               "cb4d_model_diagnostics_vol01_report.Rmd")
#' report_volume    <- 1
#' # use defaults for last 3 parameters
#' dn_results       <- file.path(tempdir(), "diagnostics_results")
#' fn_report_prefix <- "cb4d_model_report_vol01~"
#' quiet            <- FALSE
#'
#' # Run Function
#' report_diagnostics(fn_model_rda,
#'                    dn_model_rda,
#'                    format_rmd,
#'                    path_rmd,
#'                    report_volume,
#'                    dn_results,
#'                    fn_report_prefix,
#'                    quiet)
#' # open results folder (Windows only)
#' shell.exec(dn_results)
#'
#'
#' # Example 2, Select File(s), dialog box (Windows only)
#' #if (!require(tcltk)) {install.packages("tcltk")}  #install if needed
#' #library(tcltk)
#' ### get full path so have to convert for use with function
#' #path_model_rda  <- tcltk::tk_choose.files()
#' #fn_model_rda    <- basename(path_model_rda)
#' #dn_model_rda    <- dirname(path_model_rda)
#'
#' fn_model_rda     <- "do~CB3~datahub~2016-2018~2023_10_07_005~by_term2~none.rda"
#' dn_model_rda     <- system.file("extdata", package = "tidBITcore")
#'
#' # Other input parameters
#' format_rmd       <- "html"
#' path_rmd         <- file.path(system.file("rmd", package = "tidBITcore"),
#'                               "cb4d_model_diagnostics_vol02_report.Rmd")
#' report_volume    <- 2
#' # use defaults for last 3 parameters
#' dn_results       <- file.path(tempdir(), "diagnostics_results")
#' fn_report_prefix <- "cb4d_model_report_vol02~"
#' quiet            <- FALSE
#'
#' # Run Function
#' report_diagnostics(fn_model_rda,
#'                    dn_model_rda,
#'                    format_rmd,
#'                    path_rmd,
#'                    report_volume,
#'                    dn_results,
#'                    fn_report_prefix,
#'                    quiet)
#' # open results folder (Windows only)
#' shell.exec(dn_results)
#'
#' @export
report_diagnostics <- function(fn_model_rda = NULL,
                               dn_model_rda = NULL,
                               format_rmd = "html",
                               path_rmd = NULL,
                               report_volume = NULL,
                               dn_results = file.path(tempdir(),
                                                       , "diagnostics_results"),
                               fn_report_prefix = "cb4d_diagnostics~",
                               quiet = FALSE
                              ) {
  # Time ----
  tic <- Sys.time()

  # Test Inputs ----
  ## assign global variables
  run_type <- NULL

  ## check for NULL
  param_all <- list(fn_model_rda,
                    dn_model_rda,
                    format_rmd,
                    path_rmd,
                    dn_results,
                    fn_report_prefix,
                    quiet)
  if (any(sapply(param_all, is.null))) {
    msg <- "One or more parameters is NULL."
    stop(msg)
  }## IF ~ is.null

  ## results directory
  boo_dir_results <- dir.exists(file.path(dn_results))
  if(!boo_dir_results) {
    dir.create(file.path(dn_results))
  }## IF ~ boo_dir_results

  ## Combine RMD file name and path
  path_model_rda <- file.path(dn_model_rda, fn_model_rda)

  # if same length or if only 1 directory then ok
  # if more directories than file names the lengths won't match
  if (length(fn_model_rda) != length(path_model_rda)) {
    msg <- "Number of RDA file names and directory names does not match."
    stop(msg)
  }## IF ~ length

  ## format_rmd
  format_rmd <- tolower(format_rmd)

  if (format_rmd == "html") {
    fn_ext <- ".html"
  } else if (format_rmd == "docx") {
    fn_ext <- ".docx"
  } else {
    msg <- "Valid RMD formats are 'html' or 'docx'."
    stop(msg)
  }## IF ~ format_rmd

  ## RMD exists
  if (file.exists(path_rmd) == FALSE) {
    msg <- "RMD file does not exist."
    stop(msg)
  }## IF ~ file.exists

  ## Output dir exists
  # if it doesn't create it
  if (!file.exists(file.path(dn_results))) {
    dir.create(file.path(dn_results))
  }## IF ~ file.exists ~ dn_results

  # Report ----
  # Loop through all RDA files
  for (i in path_model_rda) {
    i_num <- match(i, path_model_rda)
    i_len <- length(path_model_rda)

    # user feedback
    if (!quiet) {
      msg <- paste0("\n\nWorking on item ", i_num, "/", i_len, ";\n", i, "\n")
      message(msg)
    }## IF ~ quiet

    # Skip if file does not exist
    if (!file.exists(i)) {
      if (!quiet) {
        msg <- paste0("File missing; ", i)
        message(msg)
      }## IF ~ quiet
      next
    }## IF ~ file.exists(i)

    # File name
    fn_output <- paste0(fn_report_prefix,
                        tools::file_path_sans_ext(basename(i)),
                        fn_ext)

    # Load Model and Create Objects ----
    #

    # Set Env
    env_cb4d <- .GlobalEnv

    # if (!is.null(path_obj)) {
    #   # Skip if file does not exist
    #   if (!file.exists(path_obj)) {
    #     msg <- paste0("Object script missing; ", path_obj)
    #     stop(msg)
    #   }## IF ~ file.exists(i)
    #   #
    #   # Load RDA
    #   ## assign variables in global environment so can access outside of function
    #   run_type <<- "script"
    #   i <<- i
    #   ## loads in script
    #   if (!quiet) {
    #     msg <- "Load and run script"
    #     message(msg)
    #   }## IF ~ quiet
    #   # Load Script
    #   source(path_obj)
    # } else {
    #   # (for testing)
    #   # Load RDA
    #   load(i)
    # }## IF ~ run_type)

    run_type <<- "script"
    i <<- i
    report_objects(report_volume)

    #~~~~~~~~~~~~
    # Env Notes -
    #
    # use <<- to assign variable within global env so don't need to define env
    #
    # Need to check which environment the information is created from the script
    #
    # normally need to do all work in the RMD.
    # if grab from global env with `get` then it works
    # > get("tbl01", envir = .GlobalEnv)
    # setting envir in render is easier
    #~~~~~~~~~~~~

    # Render RMD ----
    rmarkdown::render(path_rmd,
                      output_format = paste0(format_rmd, "_document"),
                      output_file = fn_output,
                      output_dir = dn_results,
                      quiet = quiet,
                      envir = env_cb4d)

    # Time, RMD ----
    if (!quiet) {
      toc_rmd <- Sys.time()
      timer_rmd <- difftime(toc_rmd, tic, units = "mins")
      msg <- paste0("\nRun Time (min), RMD:\n",
                    round(timer_rmd, 1))
      message(msg)
    }## IF ~ quiet

  }## FOR ~ i

  # Time, Total ----
  if (!quiet) {
    toc <- Sys.time()
    timer_rmd <- difftime(toc, tic, units = "mins")
    msg <- paste0("\nRun Time (min), total:\n",
                  round(timer_rmd, 1))
    message(msg)
  }## IF ~ quiet

}## FUNCTION ~ report_cb4d

