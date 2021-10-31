# Non-export helper functions

file_info <- function(file) {
  tibble(
    name = basename(file),
    type = gsub("\\.", "", strextract(file, "\\.[A-Za-z]+")),
    version = file_version(file),
    size = file_size(file),
    path = file
  )
}

file_version <- function(file) {
  tokens <- unlist(strsplit(file, split = "/"))
  number_of_tokens <- length(tokens)
  tokens[number_of_tokens - 1]
}

file_size <- function(file, n = 3) {
  file_size <- file.size(file)
  if (!is.na(file_size)) {
    out <- format(round(file_size / 10 ^ 6, n), nsmall = n)
  } else {
    out <- NA
  }
  out
}

is_positive_int <- function(x) {
  (x %% 1 == 0) & (x > 0)
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

is_file_empty <- function(file_path) {
  isTRUE(is.character(file_path) && file.exists(file_path) && file.info(file_path, extra_cols = FALSE)$size == 0)
}

is_absolute_path <- function(file_path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", file_path)
}

is_url <- function(url) {
 str_starts(url, "http://|https://|ftp://|ftps://")
}

str_wrap2 <- function(string1, string2, width = stoRy_opt("width"), indent = 0, exdent = 0) {
  padding <- stringr::str_dup(" ", indent)
  gsub(str_c("^", padding, string1), str_c(padding, bold(string1)), str_wrap(str_c(padding, string1, ": ", string2), width = width, indent = indent, exdent = exdent))
}

strwrap2 <- function(x, width, indent) {
  fansi::strwrap_ctl(x, width = max(width, 0), indent = indent, exdent = indent + 2)
}

get_underline <- function(string) {
  return(paste0(rep("=", nchar(string)), collapse = ""))
}

format_comment <- function(x, width) {
  if (length(x) == 0L) return(character())
    silver(purrr::map_chr(x, wrap, prefix = "# ", width = min(width, stoRy_opt("width"))))
}

wrap <- function(..., indent = 0, prefix = "", width) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap2(x, width - nchar_width(prefix), indent)
  wrapped <- paste0(prefix, wrapped)
  wrapped <- gsub("\U00A0", " ", wrapped)
  paste0(wrapped, collapse = "\n")
}

nchar_width <- function(x) {
  nchar(x, type = "width")
}

pre_dots <- function(x) {
  if (length(x) > 0) {
    paste0(cli::symbol$ellipsis, " ", x)
  } else {
    character()
  }
}
