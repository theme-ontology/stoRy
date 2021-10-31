#' @title Find similar stories to a given story
#' 
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `get_similar_stories` calculates the top `n` most thematically
#' similar stories to a given story.
#' 
#' @return Returns a \code{\link[tibble]{tibble}} with `top_n` rows (stories)
#' and 5 columns:
#' \tabular{ll}{
#'   `story_id`: \tab `n`-th most thematically similar story to the query
#'   story\cr
#'   `title`: \tab Reference story title\cr
#'   `description`: \tab Reference story description\cr
#'   `score`: \tab Cosine similarity score with hypergeometric test weights
#'   (if `metric = "hgt"`) or TF-IDF weights (if `metric = "tfidf"`).\cr
#'   `common_themes`: \tab List of themes common to both the query and
#'   reference story\cr
#' }
#'
#' @references
#' Paul Sheridan, Mikael Onsjö, Claudia Becerra, Sergio Jimenez, Georg Dueñas
#' (2019). \emph{An Ontology-Based Recommender System with an Application to
#' the Star Trek Television Franchise}. \emph{Future Internet}, 11(9), 182.
#' DOI: \doi{10.3390/fi11090182}
#' 
#' @param query_story A [Story()] class object defining a story of interest.
#' Thematically similar stories to this one will be returned.
#' @param background_collection A [Collection()] class object the stories in
#' which to search for similar stories to the `query_story` input story.
#'
#' If `NULL`, the collection of all stories in the actively loaded
#' \acronym{LTO} version is used.
#' @param top_n Maximum number of similar stories to report. The default is
#' `top_n=10`.
#'
#' If `Inf`, all stories in the background collection are reported.
#' @template weights-arg
#' @template explicit-arg
#' @template min_freq-arg
#' @template blacklist-arg
#' @param metric A character vector specifying the choice of weighting to use
#' in the \emph{cosine similarity} measure used to evaluate story thematic
#' similarity. 
#' Use `metric = "hgt"` for hypergeometric test P-value weights and 
#' `metric = "tfidf"` for TF-IDF weights.
#' 
#' The default specification of `metric = c("hgt", "tfidf")` results in
#' hypergeometric test P-values being used as weights.
#' @importFrom stats phyper
#' @export
#' @examples \dontrun{
#' # Retrieve the top 10 most similar stories to the classic "The Twilight
#' # Zone" series episode "Nightmare at 20,000 Feet" (1959):
#' set_lto("demo")
#' query_story <- Story$new(story_id = "tz1959e5x03")
#' result_tbl <- get_similar_stories(query_story)
#' result_tbl
#'
#' # Retrieve the top 10 most similar stories to the classic "The Twilight 
#' # Zone" series episode "Nightmare at 20,000 Feet" (1959) without taking
#' # minor themes into account:
#' set_lto("demo")
#' query_story <- Story$new(story_id = "tz1959e5x03")
#' result_tbl <- get_similar_stories(query_story, weights = list(choice = 3, major = 2, minor = 0))
#' result_tbl
#'
#' # Retrieve the top 10 most similar stories to the classic "The Twilight 
#' # Zone" series episode "Nightmare at 20,000 Feet" (1959) when implicitly
#' # featured themes are included in the similarity calculation:
#' set_lto("demo")
#' query_story <- Story$new(story_id = "tz1959e5x03")
#' result_tbl <- get_similar_stories(query_story, explicit = FALSE)
#' result_tbl
#' }
get_similar_stories = function(
  query_story,
  background_collection = NULL,
  top_n = 10,
  weights = list(choice = 3, major = 2, minor = 1),
  explicit = TRUE,
  min_freq = 1,
  blacklist = NULL,
  metric = c("hgt", "tfidf")) {
  # If `query_story` is missing, stop here
  if (is_missing(query_story)) {
    message <- get_missing_arg_msg(variable_name = "query_story")
    abort(message, class = "missing_argument")
  }
      
  # If `query_story` is not a Story object, stop here
  if (isFALSE(class(query_story) == c("Story", "R6"))) {
    message <- get_invalid_story_msg(story_name = "query_story")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `background_collection` is neither a collection nor NULL, stop here
  if (isFALSE(all(class(background_collection) == c("Collection", "R6")) || identical(background_collection, NULL))) {
    message <- get_invalid_collection_msg(collection_name = "background_collection", null_ok = TRUE)
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `top_n` is neither a positive integer nor infinity, stop here
  if (isFALSE(is_positive_int(top_n) || identical(top_n, Inf))) {
    message <- get_not_positive_integer_msg(variable = "top_n")
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

  # Set background collection to all stories, if need be
  if (is.null(background_collection)) {
    background_collection <- get_background_collection()
  }

  # Initialize background collection story theme usages tibble
  background_collection_theme_usages_tbl <- background_collection$obj_internal_tbl()

  # Add query story to background collection table, if not already a member
  if (!isTRUE(query_story$story_id() %in% pull(background_collection$component_story_ids()))) {
    background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
      add_row(query_story$obj_internal_tbl())
  }

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

  # Drop superfluous columns and make groups
  background_collection_theme_usages_tbl <- background_collection_theme_usages_tbl %>%
    select(-.data$capacity, -.data$level, -.data$explicit) %>%
    group_by(.data$story_id, .data$theme_name)

  # Calculate basic reference story statistics
  ref_story_stats_tbl <- background_collection_theme_usages_tbl %>%
    count(.data$theme_name, .data$weight, name = "k") %>%
    mutate(tf = .data$weight * .data$k) %>%
    summarise(k = sum(.data$tf, na.rm = TRUE), .groups = "keep") %>%
    group_by(.data$story_id) %>%
    mutate(n = sum(.data$k))
  themes_table <- table(as.character(ref_story_stats_tbl[["theme_name"]]))
  unique_theme_names <- names(themes_table)
  unique_theme_counts <- as.numeric(themes_table)
  N <- length(unique(ref_story_stats_tbl$story_id))

  # Calculate metric specific term weights
  if (metric == "hgt") {
    N_bar = sum(ref_story_stats_tbl$k)

    ref_theme_stats_tbl <- tibble(
      theme_name = unique_theme_names,
      K_bar = tapply(ref_story_stats_tbl$k, ref_story_stats_tbl$theme_name, sum)
    )

    ref_story_stats_tbl <- ref_story_stats_tbl %>%
      left_join(ref_theme_stats_tbl, by = "theme_name") %>%
      mutate(score = - phyper(
          q = .data$k - 1,
          m = .data$K_bar,
          n = N_bar - .data$K_bar,
          k = .data$n,
          lower.tail = FALSE,
          log.p = TRUE
        )
      ) %>%
      group_by(.data$story_id) %>%
      mutate(S = sum(.data$score ^ 2)) %>%
      ungroup() %>%
      select(-.data$k, -.data$n, -.data$K_bar)
  } else if (metric == "tfidf") {
    ref_theme_stats_tbl <- tibble(
      theme_name = unique_theme_names,
      K = unique_theme_counts
    )

    ref_story_stats_tbl <- ref_story_stats_tbl %>%
      left_join(ref_theme_stats_tbl, by = "theme_name") %>%
      mutate(score = .data$k * log(N / .data$K)) %>%
      group_by(.data$story_id) %>%
      mutate(S = sum(.data$score ^ 2)) %>%
      ungroup() %>%
      select(-.data$k, -.data$n, -.data$K)
  }

  # Retrieve query story stats
  query_story_id <- query_story$story_id()
  query_story_stats_tbl <- ref_story_stats_tbl %>%
    filter(.data$story_id == !!query_story_id)

  # Calculate main result, post-process and return
  result_tbl <- query_story_stats_tbl %>% 
    inner_join(ref_story_stats_tbl, by = "theme_name", suffix = c(".query", ".ref")) %>%
    mutate(prod_score = .data$score.query * .data$score.ref) %>% 
    group_by(.data$story_id.ref, .data$S.query, .data$S.ref) %>% 
    summarise(score = sum(.data$prod_score), common_themes = list(unique(.data$theme_name)), .groups = "keep") %>% 
    mutate(score = .data$score / sqrt(.data$S.query * .data$S.ref)) %>%
    ungroup() %>%
    rename(story_id = .data$story_id.ref) %>%
    select(-.data$S.query, -.data$S.ref) %>%
    arrange(-.data$score) %>% 
    filter(.data$story_id != query_story_id) %>% 
    slice_head(n = top_n) %>%
    left_join(get_stories_tbl() %>% select(.data$story_id, .data$title, .data$description), by = 'story_id') %>%
    relocate(.data$title, .before = "score") %>%
    relocate(.data$description, .before = "score")

  result_tbl
}

