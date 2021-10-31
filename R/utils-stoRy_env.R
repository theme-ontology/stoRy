# Non-export helper functions

get_metadata_tbl <- function() {
  if (isTRUE(stoRy_env$active_version == "demo")) {
    return(metadata_tbl)
  }
  stoRy_env$metadata_tbl
}

get_collections_tbl <- function() {
  if (isTRUE(stoRy_env$active_version == "demo")) {
    return(collections_tbl)
  }
  stoRy_env$collections_tbl
}

get_stories_tbl <- function() {
  if (isTRUE(stoRy_env$active_version == "demo")) {
    return(stories_tbl)
  }
  stoRy_env$stories_tbl
}

get_themes_tbl <- function() {
  if (isTRUE(stoRy_env$active_version == "demo")) {
    return(themes_tbl)
  }
  stoRy_env$themes_tbl
}

get_background_collection <- function() {
  if (isTRUE(stoRy_env$active_version == "demo")) {
    return(background_collection)
  }
  stoRy_env$background_collection
}

get_thematic_annotations_tbl <- function(theme_name) {
  get_stories_tbl() %>%
    select(.data$story_id, .data$date, .data$themes) %>%
    unnest(cols = .data$themes) %>%
    filter(.data$theme_name == !!theme_name) %>%
    select(-.data$theme_name)
}

vget_thematic_annotations_tbl = Vectorize(get_thematic_annotations_tbl)

get_parent_theme_names <- function(theme_name) {
  get_themes_tbl() %>%
    filter(.data$theme_name == !!theme_name) %>%
    pull(.data$parents) %>% unlist(use.names = FALSE)
}

get_ancestor_theme_names <- function(theme_name,
	                                   blacklist_theme_names = NULL,
	                                   return_self = FALSE) {
  ancestor_theme_names <- get_themes_tbl() %>%
    filter(.data$theme_name == !!theme_name) %>%
    pull(.data$ancestors) %>%
    unlist(use.names = FALSE)
  if (return_self) {
    ancestor_theme_names <- c(theme_name, ancestor_theme_names)
  }
  setdiff(ancestor_theme_names, blacklist_theme_names)
}

vget_ancestor_theme_names = Vectorize(get_ancestor_theme_names)

get_number_of_printed_entries <- function(n, number_of_entries) {
  if (is.null(n)) {
    number_of_printed_entries <- min(number_of_entries,
    	                               stoRy_opt("print_min"))
  } else {
    number_of_printed_entries <- min(n,
    	                               number_of_entries,
    	                               stoRy_opt("print_max"))
  }
  number_of_printed_entries
}
