#' @title Find the most frequently occurring themes in a collection
#' 
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `get_featured_themes()` calculates the top `m` most frequently occurring
#' themes in a collection.
#'
#' @return Returns a \code{\link[tibble]{tibble}} with `top_m` rows (themes)
#' and 6 columns:
#' \tabular{ll}{
#'   `theme_name`: \tab `m`-th most frequently occurring theme in the
#'   collection\cr
#'   `k`: \tab Number of collection stories featuring the theme\cr
#'   `k_bar`: \tab Weighted counts of the theme summed over the collection
#'   stories\cr
#'   `n`: \tab Number of stories in the collection\cr
#'   `n_bar`: \tab Sum of all weighted counts of collection themes\cr
#'   `tp`: \tab Theme weighted term proportion (i.e. `k_bar`/`n_bar`)\cr
#' }
#'
#' @details
#' The input collection of `n` stories, \eqn{S[1], \ldots, S[n]}, is 
#' represented as a weighted bag-of-words, where each \emph{choice} theme in
#' story \eqn{S[j] (j=1, \ldots, n)} is counted \code{weights$choice} times,
#' each \emph{major} theme \code{weights$major} times, and each \emph{minor}
#' theme \code{weights$choice} times.
#' 
#' @template collection-arg
#' @template top_m-arg
#' @template weights-arg
#' @template explicit-arg
#' @template min_freq-arg
#' @template blacklist-arg
#' @export
#' @examples \dontrun{
#' # Retrieve the top 10 most featured themes in "The Twilight Zone" franchise
#' # stories:
#' set_lto("demo")
#' result_tbl <- get_featured_themes()
#' result_tbl
#'
#' # Retrieve the top 10 most featured themes in "The Twilight Zone" franchise
#' # stories not including any minor level themes:
#' set_lto("demo")
#' result_tbl <- get_featured_themes(weights = list(choice = 1, major = 1, minor = 0))
#' result_tbl
#'
#' # Retrieve the top 10 most featured themes in "The Twilight Zone" (1959)
#' # television series episodes:
#' collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
#' result_tbl <- get_featured_themes(collection)
#' result_tbl
#' }
get_featured_themes = function(
  collection = NULL,
  top_m = 10,  
  weights = list(choice = 3, major = 2, minor = 1),
  explicit = TRUE,
  min_freq = 1,
  blacklist = NULL) { 
  # If `collection` is neither a collection nor NULL, stop here
  if (isFALSE(all(class(collection) == c("Collection", "R6")) || identical(collection, NULL))) {
    message <- get_invalid_collection_msg(collection_name = "collection", null_ok = TRUE)
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `top_m` is neither a positive integer nor infinity, stop here
  if (isFALSE(is_positive_int(top_m) || identical(top_m, Inf))) {
    message <- get_not_positive_integer_msg(variable = "top_m", infinity = TRUE)
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `weights` is invalid, stop here
  check_theme_level_weights(weights)

  # Restrict the analysis to themes of level in `levels`
  levels <- theme_levels()[which(unlist(weights, use.names = FALSE) > 0)]

  # If `explicit` is not TRUE/FALSE, stop here
  if (isFALSE(class(explicit) == "logical")) {
    message <- get_not_logical_msg(variable = "explicit")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `min_freq` is neither a positive integer nor infinity, stop here
  if (isFALSE(is_positive_int(min_freq))) {
    message <- get_not_positive_integer_msg(variable = "min_freq")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `blacklist` is neither a themeset nor NULL, stop here
  if (isFALSE(all(class(blacklist) == c("Themeset", "R6")) || identical(blacklist, NULL))) {
    message <- get_invalid_themeset_msg(variable = "blacklist")
    abort(message, class = "function_argument_type_check_fail")
  }

  # Set collection to background collection, if need be
  if (is.null(collection)) {
    collection <- get_background_collection()
  }

  # Initialize theme in collection story usages tibble
  collection_theme_usages_tbl <- collection$obj_internal_tbl()

  # Excise implicit theme usages, if need be
  if (isTRUE(explicit)) {
    collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
      filter(.data$explicit == !!explicit)
  }

  # Excise unused levels, if any
  if (!identical(levels, theme_levels())) {
    collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
      filter(.data$level %in% !!levels)
  }

  # Excise themes that fall below the minimum occurrence threshold, if any
  if (isTRUE(min_freq > 1)) {
    theme_counts <- table(collection_theme_usages_tbl$theme_name)
    low_frequency_themes <- theme_counts[which(theme_counts <= min_freq)]
    if (isTRUE(length(low_frequency_themes) > 0)) {
      collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
      filter(.data$theme_name %in% !!low_frequency_themes)
    }
  }

  # Excise blacklisted themes, if any
  if (!identical(blacklist, NULL)) {
    collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
      filter(!.data$theme_name %in% !!pull(blacklist$component_theme_names()))
  }

  # Add theme level weights
  collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
  mutate(weight = case_when(
    level == "choice" ~ weights$choice,
    level == "major" ~ weights$major,
    level == "minor" ~ weights$minor)
  )

  # Drop superfluous columns
  collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
      select(-.data$capacity, -.data$level, -.data$explicit)

  # Calculate main result
  result_tbl <- collection_theme_usages_tbl %>%
  mutate(
    n = length(unique(.data$story_id)),
    n_bar = sum(.data$weight)
  ) %>%
  group_by(.data$theme_name, .data$n, .data$n_bar) %>%
  summarise(
    k = length(unique(.data$story_id)),
    k_bar = sum(.data$weight, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  relocate(.data$k, .before = "n") %>%
  relocate(.data$k_bar, .before = "n") %>%
  mutate(tp = .data$k_bar / .data$n_bar) %>%
  arrange(-.data$tp) %>%
  slice_head(n = top_m)

  # Postprocession and return
  result_tbl$k_bar <- as.integer(result_tbl$k_bar)
  result_tbl$n_bar <- as.integer(result_tbl$n_bar)

  result_tbl
}


