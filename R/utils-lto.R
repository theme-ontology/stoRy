# Non-export helper functions

categories <- function() c("collections", "stories", "themes")

theme_levels <- function() c("choice", "major", "minor")

lto_json_file_names <- function(version) {
  if (isTRUE(version == "latest")) version <- get_latest_version_tag()
  paste0("lto-", version, "-", categories(), ".json")
}

defunct_versions <- function() c("v0.1.1", "v0.1.0")

base_url <- function() "https://www.themeontology.org/pub/data"

lto_repo_url <- function() "https://api.github.com/repos/theme-ontology/theming/tags"

lto_file_extensions <- function() c("\\.st\\.txt", "\\.thset\\.txt")

lto_rds_file_names <- function() {
  c(paste0(categories(), "_tbl.Rds"),
    "metadata_tbl.Rds",
    "stub_story_ids_tbl.Rds",
    "background_collection.Rds",
    "lto_file_timestamps.Rds")
}

is_lto_version_tag_valid <- function(version) {
  versions <- fetch_lto_version_tags(verbose = FALSE)
  ifelse(isTRUE((version %in% versions) || version == "latest"), TRUE, FALSE)
}

are_lto_files_cached <- function(file_names, version) {
  if (isTRUE(version == "demo")) return(TRUE)
  if (isTRUE(version == "latest")) version <- get_latest_version_tag()
  file_paths <- file.path(stoRy_cache_path(), version, file_names)
  ifelse(all(file.exists(file_paths)), TRUE, FALSE)
}

is_lto_file_cached <- function(file_name, version) {
  if (isTRUE(version == "demo")) return(TRUE)
  if (isTRUE(version == "latest")) version <- get_latest_version_tag()
  file_path <- file.path(stoRy_cache_path(), version, file_name)
  ifelse(file.exists(file_path), TRUE, FALSE)
}

get_latest_version_tag <- function() {
  versions <- fetch_lto_version_tags(verbose = FALSE)
  sort(versions, decreasing = TRUE)[1]
}

are_newest_lto_json_files_cached <- function(version) {
  cached_lto_file_timestamps_tbl <- get_cached_lto_file_timestamps_tbl(version)
  if (is.null(cached_lto_file_timestamps_tbl)) {
    return(FALSE)
  }
  website_lto_file_timestamps_tbl <- get_website_lto_file_timestamps_tbl(version)
  if (identical(cached_lto_file_timestamps_tbl$timestamp, website_lto_file_timestamps_tbl$timestamp)) {
    return(TRUE)
  }
  FALSE
}

get_website_lto_file_timestamps_tbl <- function(version) {
  inlines <- readLines(base_url())
  mypattern <- ">([^<]+[^\\s$])(\\s*)<"
  inlines <- unlist(lapply(stringr::str_match_all(string = inlines, pattern = mypattern), function(x){x[, 2]}))
  inlines <- inlines[!grepl(inlines,pattern = "nbsp")]
  lto_file_timestamps <- inlines[which(inlines %in% lto_json_file_names(version)) + 1]
  website_lto_file_timestamps_tbl <- tibble(file = lto_json_file_names(version),
                                            timestamp = lto_file_timestamps)
  website_lto_file_timestamps_tbl
}

get_website_lto_file_timestamp <- function(file_name) {
  inlines <- readLines(base_url())
  mypattern <- ">([^<]+[^\\s$])(\\s*)<"
  inlines <- unlist(lapply(stringr::str_match_all(string = inlines, pattern = mypattern), function(x){x[, 2]}))
  inlines <- inlines[!grepl(inlines,pattern = "nbsp")]
  lto_file_timestamps <- inlines[which(inlines == file_name) + 1]
}

get_cached_lto_file_timestamps_tbl <- function(version) {
  timestamps_file_path <- file.path(stoRy_cache_path(), version, "lto_file_timestamps.Rds")
  if (!file.exists(timestamps_file_path)) return(invisible(NULL))
  cached_lto_file_timestamps <- readRDS(timestamps_file_path)
  cached_lto_file_timestamps
}

handle_curl_errors <- function(response, file = NULL) {
  if (response$status_code > 201) {
    response$raise_for_status()
    if (!is.null(file)) {
      unlink(file, recursive = TRUE, force = TRUE)
    }
  }
}

download_url_with_progress_bar = function(file_url){
  pb <- progress::progress_bar$new(
    format = black("   [:bar] :percent eta: :eta"),
    clear = FALSE,
    total = download_size(file_url),
    width = min(stoRy_opt("width"), floor(0.9 * getOption("width")))
  )
  contents <- NULL
  pb_fun = function(x) {
    pb$tick(length(x))
    contents <<- c(contents, x)
  }
  
  response <- curl::curl_fetch_stream(file_url, pb_fun)
  list('response' = response, 'contents' = contents)
}

formatted_file_size <- function(file_size) format(structure(file_size, class = "object_size"), units = "auto")

download_size <- function(url) as.numeric(httr::HEAD(url)$headers$`content-length`)
