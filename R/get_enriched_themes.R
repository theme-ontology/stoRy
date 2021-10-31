#' @title Find over-represented themes in a collection
#' 
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `get_enriched_themes()` calculates the top `m` most over-represented (or
#' enriched) themes in a sub-collection of interest from a background
#' collection.
#'
#' @return Returns a \code{\link[tibble]{tibble}} with `top_m` rows (themes)
#' and 10 columns:
#' \tabular{ll}{
#'   `theme_name`: \tab `m`-th most over-represented theme in the test
#'   collection\cr
#'   `k`: \tab Number of test collection stories featuring the theme\cr
#'   `k_bar`: \tab Weighted counts of the theme summed over the test
#'   collection stories\cr
#'   `n`: \tab Number of stories in the test collection\cr
#'   `n_bar`: \tab Sum of all weighted counts of test collection themes\cr
#'   `K`: \tab Number of background collection stories featuring the theme\cr
#'   `K_bar`: \tab Weighted counts of the theme summed over the background
#'   collection stories\cr
#'   `N`: \tab Number of stories in the background collection\cr
#'   `N_bar`: \tab Sum of all weighted counts of background collection
#'   themes\cr
#'   `score`: \tab Either the negative base 10 logarithm of the Hypergeometric
#'   test (if `metric = "hgt"`) or TF-IDF (if `metric = "tfidf"`)\cr
#' }
#'
#' @details
#' The test collection of `n` stories, \eqn{S[1], \ldots, S[n]}, is 
#' represented as a weighted bag-of-words, where each \emph{choice} theme in
#' story \eqn{S[j] (j=1, \ldots, n)} is counted \code{weights$choice} times,
#' each \emph{major} theme \code{weights$major} times, and each \emph{minor}
#' theme \code{weights$choice} times.
#'
#' The background collection of `N` stories, \eqn{S[1], \ldots, S[N]}, is a
#' superset of the test collection that is likewise represented as a weighted
#' bag-of-words.
#'
#' Theme enrichment scores are calculated according to the hypergeometric test
#' by default. Set `metric = "tfidf"` to use TF-IDF weights for the enrichment
#' scores.
#'
#' @references
#' Mikael Onsjö, Paul Sheridan (2020). Theme Enrichment Analysis: A
#' Statistical Test for Identifying Significantly Enriched Themes in a List of
#' Stories with an Application to the Star Trek Television Franchise.
#' \emph{Digital Studies/le Champ Numérique}, 10(1), 1. DOI:
#' \doi{10.16995/dscn.316}
#' 
#' @param test_collection A [Collection()] class object of stories to assay
#' for over-represented themes. 
#' @param background_collection A [Collection()] class object the stories of
#' are a superset of the `test_collection` stories.
#'
#' If `NULL`, the collection of all stories in the actively loaded
#' \acronym{LTO} version is used.
#' @template top_m-arg
#' @template weights-arg
#' @template explicit-arg
#' @template min_freq-arg
#' @template blacklist-arg
#' @param metric A character vector specifying the choice of scoring function.
#' Use `metric = "hgt"` for the \emph{hypergeometric test}, and
#' `metric = "tfidf"` for \emph{term frequency-inverse document frequency}.
#' The default specification of `metric = c("hgt", "tfidf")` results in
#' the hypergeometric test being used in the analysis.
#' @importFrom stats phyper
#' @export
#' @examples \dontrun{
#' # Retrieve the top 10 most enriched themes in "The Twilight Zone" (1959)
#' # series episodes with all demo version stories as background:
#' set_lto("demo")
#' test_collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
#' result_tbl <- get_enriched_themes(test_collection)
#' result_tbl
#'
#' # Run the same analysis on "The Twilight Zone" (1959) series without
#' # including minor level themes:
#' result_tbl <- get_enriched_themes(test_collection, weights = list(choice = 1, major = 1, minor = 0))
#' result_tbl
#' }
get_enriched_themes = function(
  test_collection,
  background_collection = NULL,
  top_m = 10,
  weights = list(choice = 3, major = 2, minor = 1),
  explicit = TRUE,
  min_freq = 1,
  blacklist = NULL,
  metric = c("hgt", "tfidf")) {
  # If `test_collection` is not a Collection object, stop here
  if (isFALSE(class(test_collection) == c("Collection", "R6"))) {
    message <- get_invalid_collection_msg(collection_name = "test_collection")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `background_collection` is neither a collection nor NULL, stop here
  if (isFALSE(all(class(background_collection) == c("Collection", "R6")) || identical(background_collection, NULL))) {
    message <- get_invalid_collection_msg(collection_name = "background_collection", null_ok = TRUE)
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `top_m` is neither a positive integer nor infinity, stop here
  if (isFALSE(is_positive_int(top_m) || identical(top_m, Inf))) {
    message <- get_not_positive_integer_msg(variable = "top_m")
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

  # If `metric` is invalid, stop here
  metrics <- c("hgt", "tfidf")
  if (isFALSE(all(metric %in% metrics) && length(metric) <= length(metrics))) {
    message <- get_invalid_element_msg(variable = "metric", choices = metrics)
    abort(message, class = "function_argument_type_check_fail")
  } 

  # If `metric` is unspecified, set it to be TF-IDF
  if (isTRUE(identical(metric, metrics))) {
    metric <- "hgt"
  }

  # Initialize test collection story theme usages tibble
  test_collection_theme_usages_tbl <- test_collection$obj_internal_tbl()

  # Excise implicit theme usages, if need be
  if (isTRUE(explicit)) {
    test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
      filter(.data$explicit == !!explicit)
  }

  # Excise unused levels, if any
  if (!identical(levels, theme_levels())) {
    test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
      filter(.data$level %in% !!levels)
  }

  # Excise themes that fall below the minimum occurrence threshold, if any
  if (isTRUE(min_freq > 1)) {
    theme_counts <- table(test_collection_theme_usages_tbl$theme_name)
    low_frequency_themes <- theme_counts[which(theme_counts <= min_freq)]
    if (isTRUE(length(low_frequency_themes) > 0)) {
      test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
      filter(.data$theme_name %in% !!low_frequency_themes)
    }
  }

  # Excise blacklisted themes, if any
  if (!identical(blacklist, NULL)) {
    test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
      filter(!.data$theme_name %in% !!pull(blacklist$component_theme_names()))
  }

  # Add theme level weights
  test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
  mutate(weight = case_when(
    level == "choice" ~ weights$choice,
    level == "major" ~ weights$major,
    level == "minor" ~ weights$minor)
  )

  # Drop superfluous columns
  test_collection_theme_usages_tbl <- test_collection_theme_usages_tbl %>%
      select(-.data$capacity, -.data$level, -.data$explicit)

  # Set background collection to all stories, if need be
  if (is.null(background_collection)) {
    background_collection <- get_background_collection()
  }

  # Initialize background collection story theme usages tibble
  background_collection_theme_usages_tbl <- background_collection$obj_internal_tbl()

  # Excise implicit theme usages, if need be
  if (isTRUE(explicit)) {
    background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      filter(.data$explicit == !!explicit)
  }

  # Excise unused levels, if any
  if (!identical(levels, theme_levels())) {
    background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      filter(.data$level %in% !!levels)
  }

  # Excise themes that fall below the minimum occurrence threshold, if any
  if (isTRUE(min_freq > 1)) {
    theme_counts <- table(background_collection_theme_usages_tbl$theme_name)
    low_frequency_themes <- theme_counts[which(theme_counts <= min_freq)]
    if (isTRUE(length(low_frequency_themes) > 0)) {
      background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      filter(.data$theme_name %in% !!low_frequency_themes)
    }
  }

  # Excise blacklisted themes, if any
  if (!identical(blacklist, NULL)) {
    background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      filter(!.data$theme_name %in% !!pull(blacklist$component_theme_names()))
  }

  # Add theme level weights
  background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
  mutate(weight = case_when(
    level == "choice" ~ weights$choice,
    level == "major" ~ weights$major,
    level == "minor" ~ weights$minor)
  )

  # Drop superfluous columns
  background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      select(-.data$capacity, -.data$level, -.data$explicit)

  # Calculate main result
  result_tbl <- test_collection_theme_usages_tbl %>%
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
  full_join(
    background_collection_theme_usages_tbl %>%
    mutate(
      N = length(unique(.data$story_id)),
      N_bar = sum(.data$weight)
    ) %>%
    group_by(.data$theme_name, .data$N, .data$N_bar) %>%
    summarise(
      K = length(unique(.data$story_id)),
      K_bar = sum(.data$weight, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    relocate(.data$K, .before = "N") %>%
    relocate(.data$K_bar, .before = "N"),
    by = "theme_name"
  )

  # Add scores
  if (metric == "hgt") {
    result_tbl <- result_tbl %>%
    mutate(score = hgt <- - phyper(
        q = .data$k_bar - 1,
        m = .data$K_bar,
        n = .data$N_bar - .data$K_bar,
        k = .data$n_bar,
        lower.tail = FALSE,
        log.p = TRUE
      )
    )
  } else if (metric == "tfidf") {
    result_tbl <- result_tbl %>%
      mutate(score = -.data$k_bar * log(.data$K / .data$N))
  }

  # Post-process and return
  result_tbl$k_bar <- as.integer(result_tbl$k_bar)
  result_tbl$n_bar <- as.integer(result_tbl$n_bar)
  result_tbl$K_bar <- as.integer(result_tbl$K_bar)
  result_tbl$N_bar <- as.integer(result_tbl$N_bar)
  result_tbl <- result_tbl %>%
    filter(.data$k >= min_freq) %>%
    arrange(desc(.data$score)) %>%
    slice_head(n = top_m)

  result_tbl
}



