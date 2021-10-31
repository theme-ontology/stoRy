op.stoRy <- list(
  stoRy.width = 78L,
  stoRy.print_min = 10L,
  stoRy.print_max = 100L
)

#' @title Set \pkg{stoRy} package global options
#' 
#' @description
#' `stoRy_opt()` sets \pkg{stoRy} package global options.
#'
#' @details
#' The package options control the formatting of the [Story()], [Theme()], 
#' [Collection()], and [Themeset()] R6 classes when printing to console. The
#' options are as follows:
#' \tabular{ll}{
#'   `width`: \tab The column width in characters of printed to console
#'   output (78 characters by default)\cr
#'   `print_min`: \tab The minimum number of entries to print to console (10
#'   characters by default)\cr
#'   `print_max`: \tab The maximum number of entries to print to console (100
#'   characters by default)\cr
#' }
#' 
#' @param x A character string holding an option name. The possible values are
#' `"width"`, `"print_min"`, and `"print_max"`.
#'
#' @seealso Use [Story()] to initialize an LTO thematically annotated story.
#' @seealso Use [Theme()] to initialize an LTO theme.
#' @seealso Use [Collection()] to initialize an collection of LTO thematically
#'   annotated stories.
#' @seealso Use [Themeset()] to initialize a set of related LTO themes.
#'
#' @export
#' @examples \dontrun{
#' # Check the current option values:
#' stoRy_opt("width")
#' stoRy_opt("print_min")
#' stoRy_opt("print_max")
#'
#' # Set the column width to 120 characters:
#' options(stoRy.width = 120L) 
#' 
#' # Set the minimum number of printed entries to be 25:
#' options(stoRy.print_min = 25L) 
#'  
#' # Set the maximum number of printed entries to be 250:
#' options(stoRy.print_max = 250L)
#' }
stoRy_opt <- function(x) {
  x_stoRy <- paste0("stoRy.", x)
  res <- getOption(x_stoRy)
  if (!is.null(res)) {
    return(res)
  }

  op.stoRy[[x_stoRy]]
}

