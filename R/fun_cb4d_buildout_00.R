# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Clear the Viewer Pane in RStudio
#'
#' @description Clears the Viewer pane in RStudio by creating and opening a
#'   blank HTML file. This function is useful for resetting the Viewer pane to a
#'   clean state, especially after displaying multiple visualizations or HTML
#'   outputs in sequence. It leverages the `rstudioapi` package to interact with
#'   the RStudio environment, making it specifically tailored for use within
#'   RStudio IDE.
#'
#' @details The function generates a temporary directory, creates a blank HTML
#'   file within this directory, and then uses the RStudio Viewer to open this
#'   blank file, effectively clearing any previous content displayed in the
#'   Viewer pane. This is particularly handy in interactive R sessions and
#'   RMarkdown documents when an empty Viewer pane is desired for either
#'   aesthetic or functional reasons.
#'
#' @return Invisible `NULL`. The primary effect is the side effect of clearing
#'   the Viewer pane.
#'
#' @examples
#' \dontrun{
#' cv()
#' }
#'
#' @importFrom rstudioapi viewer
#'
#' @export
#'
#' @seealso \code{\link[rstudioapi]{viewer}}, for details on how to display local web content in the Viewer pane.
#'
#' @importFrom rstudioapi viewer
#'
#'
cv <- function() {

  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile)

} # end ~ function: cv


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Output Formatted Table Using Flextable
#'
#' @description Outputs a formatted table based on the provided dataset.
#'   Utilizes the `flextable` package to create highly customizable tables with
#'   options for title, font, size, and theme adjustments. This function is
#'   designed for both interactive use within RStudio and integration within R
#'   Markdown documents, offering a versatile solution for displaying data in a
#'   visually appealing format.
#'
#' @details The function offers several customization options:
#' \itemize{
#'   \item Table title and prefix label can be specified, allowing for
#'   descriptive headers above the table.
#'   \item Font name and size customization enable adherence to specific styling
#'   guidelines or preferences.
#'   \item A selection of table themes (`"box"`, `"vanilla"`, `"booktabs"`)
#'   allows for varied stylistic presentations of the data.
#' }
#' Additionally, the function detects the running environment to appropriately
#' render the table for interactive previews or for embedding within R Markdown
#' documents. This adaptability makes it an effective tool for data presentation
#' in diverse contexts.
#'
#' @param data A dataframe or tibble to be displayed as a formatted table.
#' @param tbl_title (Optional) The title of the table to be displayed above it.
#' @param tbl_pre_label (Optional) Prefix label for the table title, e.g., "Table:".
#' @param tbl_font_name (Optional) The font name to be used throughout the table.
#' @param tbl_font_size (Optional) The font size to be applied to the table text.
#' @param tbl_theme (Optional) The theme to apply to the table. Supported themes
#'  include "box", "vanilla", and "booktabs".
#'
#' @return A `flextable` object representing the formatted table. This object
#' can be printed in the R console, rendered in the RStudio Viewer, or included
#' in R Markdown documents.
#'
#' @examples
#' \dontrun{
#' tblFT1(head(iris), tbl_title = "Summary of Iris Data", tbl_theme = "box")
#' }
#'
#' @importFrom flextable flextable fontsize font padding set_caption theme_box theme_vanilla theme_booktabs
#' @importFrom rstudioapi isAvailable getActiveDocumentContext
#'
#' @export
#'
#' @seealso \code{\link[flextable]{flextable}}, for more details on creating and customizing flextables.
#' \code{\link[officer]{run_autonum}}, for automatically numbering tables and figures in R Markdown.
#'
#' @importFrom flextable flextable fontsize font padding set_caption theme_box theme_vanilla theme_booktabs flextable_to_rmd
#' @importFrom officer run_autonum
#' @importFrom rstudioapi isAvailable getActiveDocumentContext
#'
#'
tblFT1 <- function(data
  , tbl_title = NA
  , tbl_pre_label = NA   # "Table: "
  , tbl_font_name = NA    # "Calibri"
  , tbl_font_size = NA    # 11
  , tbl_theme = NA       # "box"
) {

  # ----< Interactive >---
  isInteractive <- rstudioapi::isAvailable()
  ctxt <- rstudioapi::getActiveDocumentContext()
  isRmd <- ifelse(grepl("\\.Rmd$", ctxt$path), TRUE, FALSE)

  # ----< create flextable >---
  FT <- flextable(data) %>%
    padding(padding = 1, part = "all")

  # %>%
  #   colformat_int(big.mark = "") %>%
  #   colformat_double(big.mark = "")

  # ----< customize font name >---
  if (!is.na(tbl_font_name)) {
    FT <- font(FT, fontname = tbl_font_name, part = "all")
  }

  # ----< customize font size >---
  if (!is.na(tbl_font_size)) {
    FT <- fontsize(FT, size = tbl_font_size, part = "all")
  }

  # ----< customize table theme >---
  stopifnot(tbl_theme %in% c(NA, "box", "vanilla", "booktabs"))
  if (!is.na(tbl_theme)) {
    if (tbl_theme == "box") {
      FT <- theme_box(FT)
    } else if (tbl_theme == "vanilla") {
      FT <- theme_vanilla(FT)
    } else if (tbl_theme == "booktabs") {
      FT <- theme_booktabs(FT)
    }
  }

  # ----< customize table title >---
  if (!is.na(tbl_title)) {
    FT <- set_caption(FT, caption = tbl_title
      , autonum = run_autonum(seq_id = "tab"
                              , pre_label = tbl_pre_label
                              , bkm = "anytable"))
  }

  # # ----< customize output >---
  if (isInteractive) {
    print(FT, preview = "html")
  } else {
    flextable_to_rmd(FT)
  }

  return(FT)

}## FUN ~ tblFT1


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Append Variables to a List
#'
#' @description Dynamically appends variables to an existing list based on the
#'   names provided in the input vector `v`. This function evaluates the names
#'   in `v` within the current environment and appends the corresponding objects
#'   to the list `listvar`. It's useful for aggregating multiple variables into
#'   a single list structure dynamically, facilitating data management and
#'   manipulation in complex workflows.
#'
#' @details The function iterates over the vector `v`, evaluating each name to
#'   find the corresponding object in the current environment. These objects are
#'   then appended to the list `listvar` under their original names. This
#'   approach allows for flexible and dynamic construction of lists from
#'   separate variables without manually specifying each one. It is particularly
#'   handy in scenarios where variable names or objects are generated
#'   programmatically, and direct aggregation into a list simplifies subsequent
#'   data processing steps.
#'
#' @param listvar A list to which variables will be appended. If not provided,
#'   an empty list is initialized.
#' @param v A character vector containing the names of variables to append to
#'   `listvar`.
#'
#' @return A list containing the original elements of `listvar` with the
#'   additional variables appended.
#'
#' @examples
#' # Define some variables
#' a <- 1; b <- 2; c <- "three"
#' # Initialize an empty list
#' my_list <- list()
#' # Append variables a and b to the list
#' my_list <- stow(my_list, c("a", "b"))
#' print(my_list)
#'
#' @export
#'
#' @seealso \code{\link{list}}, for creating lists.
#' \code{\link{eval}}, \code{\link{parse}}, for details on evaluating and parsing expressions.
#'
#'
stow <- function(listvar=list(), v=NA) {

  for (var in v) {
    listvar[[var]] <- eval(parse(text=var))
  }

  return(listvar)

} # end ~ function: stow

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Extract and Assign List Components to the Parent Environment
#'
#' @description Extracts specified components from a list or table and assigns
#'   them to variables in the parent environment. This function facilitates the
#'   direct use of list or table components by creating variables in the current
#'   scope, simplifying access to nested data structures and enhancing code
#'   readability and efficiency.
#'
#' @details `pry` iterates over the vector `v`, extracting each specified
#'   component from `listvar` and assigning it to a variable of the same name in
#'   the parent environment. This process allows for selective extraction of
#'   relevant data from complex or deeply nested lists or tables, making it
#'   easier to work with specific elements directly in the current script or
#'   analysis context. The function is particularly useful in scenarios where
#'   data stored in a list needs to be distributed across multiple variables for
#'   individual processing or analysis steps.
#'
#' @param listvar The target list or table from which components will be
#'   extracted.
#' @param v A character vector of component names in `listvar` to be extracted
#'   and assigned to variables in the parent environment.
#'
#' @return Invisibly returns `NULL`. The primary effect is the side effect of
#'   creating new variables in the parent environment corresponding to the
#'   extracted list components.
#'
#' @examples
#' my_list <- list(a = 1, b = 2, c = "three")
#' # Extract components a and c into the parent environment
#' pry(my_list, c("a", "c"))
#' # Now `a` and `c` are directly accessible
#' print(a)
#' print(c)
#'
#' @export
#'
#' @seealso \code{\link{list}}, for creating and manipulating list objects.
#' \code{\link{list2env}}, for converting lists into environments.
#'
#'
pry <- function(listvar, v=NA) {

  if (any(is.na(v))) {
    v <- names(v)
  }

  for(nam in v) {eval(parse(text=paste0(nam," <- listvar$",nam)))}
  rm(nam, listvar, v)
  argList <- grab_function_arguments()   # create list of function arguments
  invisible( list2env(argList, parent.frame() ) )

} # end ~ function: pry



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Grab All Function Arguments
#'
#' @description This function captures all arguments passed to the calling
#' function, including those specified via `...` (ellipsis). It is useful for
#' debugging, logging, or functions that need to programmatically process their
#' own arguments.
#'
#' @details The function retrieves the names and values of all arguments in the
#' parent frame, which corresponds to the environment from which it was called.
#' This includes handling both explicitly named arguments and those passed
#' through `...`. The solution is particularly useful in scenarios where a
#' function's behavior needs to be dynamically adjusted based on the arguments
#' it receives.
#'
#' Derived from a solution on [Stack Overflow](https://stackoverflow.com/questions/66329835/using-r-how-to-get-all-parameters-passed-into-a-function-with-their-values).
#'
#' @examples
#' \dontrun{
#' my_function <- function(z, pi_0 = 0.3, families = list(), ...) {
#'   args <- grab_function_arguments()
#'   names(args)  # Returns the names of all arguments
#'   return(args)  # Returns a list of all arguments
#' }
#'
#' X <- my_function(z = 4
#'                 , a = 345
#'                 , families = list(a = 34, b = 545)
#'                 , myList = list(a = 34, b = 545))
#' print(X)
#' }
#'
#' @return A list containing the names and values of all arguments passed to the
#'  calling function. Named arguments and those supplied through `...` are
#'  included.
#'
#' @keywords internal
#'
#' @export
#'
#'
grab_function_arguments <- function() {

  pf <- parent.frame()
  args_names <- ls(envir = pf, all.names = TRUE, sorted = FALSE)
  if("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = pf)
  }  else {
    dots = list()
  }
  args_names <- sapply(setdiff(args_names, "..."), as.name)
  if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = pf)
  } else {
    not_dots <- list()
  }
  out <- c(not_dots, dots)
  out[names(out) != ""]

} # end ~ function: grab_function_arguments

