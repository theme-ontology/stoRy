#' @title Find clusters of similar stories
#' 
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `get_story_clusters` classifies the stories in a collection according to
#' thematic similarity.
#' 
#' @return Returns a \code{\link[tibble]{tibble}} with `r` rows (story
#' clusters) and 4 columns:
#' \tabular{ll}{
#'   `cluster_id`: \tab Story cluster integer ID\cr
#'   `stories`: \tab A tibble of stories comprising the cluster\cr
#'   `themes`: \tab A tibble of themes common to the clustered stories\cr
#'   `size`: \tab Number of stories in the cluster\cr
#' }
#'
#' @details
#' The input collection of `n` stories, \eqn{S[1], \ldots, S[n]}, is 
#' represented as a weighted bag-of-words, where each \emph{choice} theme in
#' story \eqn{S[j] (j=1, \ldots, n)} is counted \code{weights$choice} times,
#' each \emph{major} theme \code{weights$major} times, and each \emph{minor}
#' theme \code{weights$choice} times.
#'
#' The function classifies the stories according to thematic similarity
#' using the Iterative Signature Algorithm (ISA) biclustering algorithm as
#' implemented in the \code{isa2} R package. The clusters are "soft" meaning
#' that a story can appear in multiple clusters.
#'
#' Install \code{isa2} package by running the command
#' \code{install.packages(\"isa2\")} before calling this function.
#'
#' @references
#' Gábor Csárdi, Zoltán Kutalik, Sven Bergmann (2010). Modular analysis of
#' gene expression data with R. \emph{Bioinformatics}, 26, 1376-7.
#'
#' Sven Bergmann, Jan Ihmels, Naama Barkai (2003). Iterative signature
#' algorithm for the analysis of large-scale gene expression data. 
#' \emph{Physical Review E}, 67, 031902.
#'
#' Gábor Csárdi (2017). isa2: The Iterative Signature Algorithm. R package 
#' version 0.3.5. \url{https://cran.r-project.org/package=isa2}
#'
#' @template collection-arg
#' @template weights-arg
#' @template explicit-arg
#' @template min_freq-arg
#' @param min_size Minimum cluster size. The default is `min_size=3`.
#' @template blacklist-arg
#' @export
#' @examples \dontrun{
#' # Cluster "The Twilight Zone" franchise stories according to thematic
#' # similarity:
#' library(dplyr)
#' set_lto("demo")
#' set.seed(123)
#' result_tbl <- get_story_clusters()
#' result_tbl
#'
#' # Explore a cluster of stories related to traveling back in time:
#' cluster_id <- 3
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#'
#' # Explore a cluster of stories related to mass panics:
#' cluster_id <- 5
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#'
#' # Explore a cluster of stories related to executions:
#' cluster_id <- 7
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#'
#' # Explore a cluster of stories related to space aliens:
#' cluster_id <- 10
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#'
#' # Explore a cluster of stories related to old people wanting to be young:
#' cluster_id <- 11
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#'
#' # Explore a cluster of stories related to wish making:
#' cluster_id <- 13
#' pull(result_tbl, stories)[[cluster_id]]
#' pull(result_tbl, themes)[[cluster_id]]
#' }
get_story_clusters = function(
  collection = NULL,
  weights = list(choice = 3, major = 2, minor = 1),
  explicit = TRUE,
  min_freq = 1,
  min_size = 3,
  blacklist = NULL) {
  # If `collection` is neither a collection nor NULL, stop here
  if (isFALSE(all(class(collection) == c("Collection", "R6")) || identical(collection, NULL))) {
    message <- get_invalid_collection_msg(collection_name = "collection", null_ok = TRUE)
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

  # If `min_size` is neither a positive integer nor infinity, stop here
  if (isFALSE(is_positive_int(min_size))) {
    message <- get_not_positive_integer_msg(variable = "min_size")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `blacklist` is neither a themeset nor NULL, stop here
  if (isFALSE(all(class(blacklist) == c("Themeset", "R6")) || identical(blacklist, NULL))) {
    message <- get_invalid_themeset_msg(variable = "blacklist")
    abort(message, class = "function_argument_type_check_fail")
  }

  # Set collection to all stories, if need be
  if (is.null(collection)) {
    collection <- get_background_collection()
  }

  # Initialize collection story theme usages tibble
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
    high_frequency_theme_names <-
      names(theme_counts[which(theme_counts >= min_freq)])
    if (isTRUE(length(high_frequency_theme_names) > 0)) {
      collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
        filter(.data$theme_name %in% high_frequency_theme_names)
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

  # Drop superfluous columns and make groups
  collection_theme_usages_tbl <- collection_theme_usages_tbl %>%
    select(-.data$capacity, -.data$level, -.data$explicit)

  # Initialize theme in story matrix
  theme_names <- unique(collection_theme_usages_tbl$theme_name)
  M <- length(theme_names)
  story_ids <- collection_theme_usages_tbl$story_id
  N <- length(story_ids)
  data <- collection_theme_usages_tbl %>%
    arrange(.data$theme_name, .data$story_id) %>%
    pivot_wider(
      names_from = .data$story_id, 
      values_from = .data$weight,
      values_fill = 0
    ) %>%
    column_to_rownames(var = 'theme_name') %>%
    as.matrix()
  
  # If `isa2` package is installed calculate story clusters;
  # otherwise stop here
  if(requireNamespace("isa2", quietly = TRUE)) {
    isa_result <- isa2::isa(data)
  } else {
    message <- str_glue(
      "The `isa2` package was not found.\n",
      "{col_red(symbol$cross)} The `isa2` package must be installed to run this function.\n",
      "{col_yellow(symbol$info)} Run 'install.packages(\"isa2\")' to install the package."
    )
    abort(message)
  }

  # Process results and return 
  no_of_clusters <- ncol(isa_result$columns)
  cluster_sizes <- numeric(no_of_clusters)
  stories_lst <- vector("list", length = no_of_clusters)
  themes_lst <- vector("list", length = no_of_clusters)
  story_ids <- colnames(data)
  theme_names <- rownames(data)
  for (i in 1 : no_of_clusters) {
    col_indices <- which(isa_result$columns[, i] > 0)
    cluster_size <-  length(col_indices)
    cluster_sizes[i] <- cluster_size
    if (isTRUE(cluster_size > 0)) {
      stories_lst[[i]] <- story_ids[col_indices]
    }
    row_indices <- which(isa_result$rows[, i] > 2 / 3)
    if (isTRUE(length(row_indices) > 0)) {
      themes_lst[[i]] <- theme_names[row_indices]
    }
  }
  sig_cluster_indices <- which(cluster_sizes >= min_size)
  no_of_sig_clusters <- length(sig_cluster_indices)
  sig_stories_lst <- vector("list", length = no_of_sig_clusters)
  sig_themes_lst <- vector("list", length = no_of_sig_clusters)
  cluster_id <- 1
  for (i in sig_cluster_indices) {
    cluster_story_ids <- stories_lst[[i]]
    cluster_theme_names <- themes_lst[[i]]
    sig_stories_lst[[cluster_id]] <- stories_tbl %>%
      filter(.data$story_id %in% cluster_story_ids)
    sig_themes_lst[[cluster_id]] <- themes_tbl %>%
      filter(.data$theme_name %in% cluster_theme_names)
    cluster_id <- cluster_id + 1
  }

  result_tbl <- tibble(
    cluster_id = 1 : no_of_sig_clusters,
    stories = sig_stories_lst,
    themes = sig_themes_lst,
    size = unlist(lapply(stories_lst[sig_cluster_indices], length))
  )

  result_tbl
}
