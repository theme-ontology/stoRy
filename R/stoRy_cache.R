#' Manage cached files
#'
#' @export
#' @name stoRy_cache
#'
#' @description
#' Manage \pkg{stoRy} package cached files.
#'
#' `stoRy_cache_path()` returns the file path where \pkg{stoRy} package files
#'   are cached.
#'
#' `stoRy_cache_details()` returns a tibble of cached file names with
#'   accompanying metadata. File size is in MB.
#'
#' `delete_lto_version_cached_files()` delete cached files associated with an
#'   \acronym{LTO} version.
#'
#' `delete_stoRy_cached_files()` delete cached files. Takes a vector of file
#'   path strings as input.
#'
#' `delete_all_cached_stoRy_files()` clear cache.
#'
#' `print_stoRy_cache_info()` print to console cache contents.
#'
#' @template version-arg
#' @param files A list of file path strings to delete from cache.
#' @param force Set to TRUE to force delete files.
#' @template verbose-arg
#'
#' @examples \dontrun{
#' # list files in cache
#' stoRy_list_cached_files()
#'
#' # List info for all files
#' print_stoRy_cache_info()
#'
#' # delete all files in cache
#' # stoRy_delete_all_cached_files()
#' }

#' @export
#' @rdname stoRy_cache
stoRy_cache_path <- function() rappdirs::user_cache_dir("stoRy")

#' @export
#' @rdname stoRy_cache
stoRy_cache_details <- function() {
  files <- list.files(stoRy_cache_path(), ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
  bind_rows(lapply(files, file_info))
}

#' @export
#' @rdname stoRy_cache
delete_lto_version_cached_files <- function(
  version,
  force = TRUE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # Set path to location of `version` files  
  base_path <- file.path(stoRy_cache_path(), version)
  
  # If `version` does not corresponds to a valid LTO version tag, stop here
  if(!is_lto_version_tag_valid(version)) {
    message <- get_invalid_lto_version_msg(version)
    abort(message, class = "lto_version_tag_not_found")
  }

  # Delete all files associated with this version
  files <- file.path(base_path, list.files(base_path))
  unlink(base_path, force = force, recursive = TRUE)
  if (verbose) {
    for (i in seq_along(files)) {
      cli_text("Deleted {.file {files[i]}}")
    }
  }
}

#' @export
#' @rdname stoRy_cache
delete_stoRy_cached_files <- function(files, force = TRUE) {
  # If `files` is missing, stop here
  if (is_missing(files)) {
    message <- get_missing_arg_msg(variable_name = "files")
    abort(message, class = "missing_argument")
  }

  if (!all(file.exists(files))) {
    stop("These files don't exist or can't be found: \n",
         strwrap(files[!file.exists(files)], indent = 5), call. = FALSE)
  }
  unlink(files, force = force, recursive = TRUE)
}

#' @export
#' @rdname stoRy_cache
delete_all_cached_stoRy_files <- function(force = TRUE) {
  files <- list.files(stoRy_cache_path(), ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
  unlink(files, force = force, recursive = TRUE)
}

#' @export
#' @rdname stoRy_cache
print_stoRy_cache_info <- function() {
  files_tbl <- stoRy_cache_details()
  cat("<stoRy cached files>", sep = "\n")
  cat(sprintf("  base directory: %s\n", stoRy_cache_path()), sep = "\n")
  for (i in seq_along(files_tbl)) {
    cat(paste0("  name: ", basename(files_tbl$name[i])), sep = "\n")
    cat(paste0("  path: ", files_tbl$path[i]), sep = "\n")
    cat(paste0("  size: ", files_tbl$size[i], " MB"), sep = "\n")
    cat("\n")
  }
}
