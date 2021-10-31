#' Parse LTO text files
#'
#' @export
#' @name text_formatting
#'
#' @description
#' Internal functions for parsing LTO raw text files.
#'
#' `remove_wordwrap()` removes single newline characters (i.e. "\\n") from
#' string, but leaves double newlines (i.e. "\\n\\n").
#'
#' @param text A character string.
#'
#' @keywords internal
#' @examples \dontrun{
#' # Remove word wrap from text blocks:
#' text <- "Themes related to people living on well beyond what
#' is considered to be a
#'
#' normal human lifespan."
#' remove_wordwrap(text)
#' } 

#' @export
#' @rdname text_formatting
remove_wordwrap <- function(text) {
  lines <- unlist(str_split(text, pattern = "\n"))
  dewordwraped_text_block <- character(0)
  dewordwraped_text_blocks <- list()

  for (line in lines) {
    line <- str_trim(line)

    if (identical(line, "")) {
      if (!identical(dewordwraped_text_block, character(0))) {
        dewordwraped_text_blocks <- append(
          dewordwraped_text_blocks,
          str_c(dewordwraped_text_block, collapse = " ")
        )
      }
      dewordwraped_text_block <- character(0)
    } else {
      dewordwraped_text_block <- append(dewordwraped_text_block, line)
    }
  }

  if (!identical(dewordwraped_text_block, character(0))) {
    dewordwraped_text_blocks <- append(
      dewordwraped_text_blocks,
      str_c(dewordwraped_text_block, collapse = " ")
    )
  }

  str_c(dewordwraped_text_blocks, collapse = "\n\n")
}
