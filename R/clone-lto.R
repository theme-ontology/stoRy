#' Clone LTO data
#'
#' @export
#' @name clone-lto
#'
#' @description
#' `r lifecycle::badge('maturing')`
#' 
#' Clone internally stored active \acronym{LTO} version data.
#'
#' `clone_active_themes_tbl()` returns a tibble of active \acronym{LTO}
#'  version themes.
#'
#' `clone_active_stories_tbl()` returns a tibble of active \acronym{LTO}
#'  version stories.
#'
#' `clone_active_collections_tbl()` returns a tibble of active \acronym{LTO}
#'  version collections.
#'
#' `clone_active_metadata_tbl()` returns a tibble of active \acronym{LTO}
#'  version metadata.
#'
#' @seealso Run [lto-demo] to view the \acronym{LTO} demo data help page.
#' @seealso Run [lto] to find out how to load \acronym{LTO} versions.
#'
#' @examples \dontrun{
#' # Make copies of the LTO demo data:
#' set_lto("demo")
#' themes_tbl <- clone_active_themes_tbl()
#' stories_tbl <- clone_active_stories_tbl()
#' collections_tbl <- clone_active_collections_tbl()
#' metadata_tbl <- clone_metadatae_stories_tbl()
#' }

#' @export
#' @rdname clone-lto
clone_active_themes_tbl <- function() {
  get_themes_tbl()
}

#' @export
#' @rdname clone-lto
clone_active_stories_tbl <- function() {
  get_stories_tbl()
}

#' @export
#' @rdname clone-lto
clone_active_collections_tbl <- function() {
  get_collections_tbl()
}

#' @export
#' @rdname clone-lto
clone_active_metadata_tbl <- function() {
  get_metadata_tbl()
}
