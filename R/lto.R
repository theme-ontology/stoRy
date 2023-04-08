#' Work with LTO versions
#'
#' @export
#' @name lto
#'
#' @description
#' `r lifecycle::badge('maturing')`
#' 
#' Download, configure, view metadata about, and navigate among different
#' \acronym{LTO} versions.
#'
#' `which_lto()` returns a length-one character vector corresponding to the
#'   active LTO version. This is the version that is loaded into the stoRy
#'   package environment. It is the `demo` version by default.
#'
#' `print_lto()` prints basic \acronym{LTO} version metadata to console.
#'
#' `fetch_lto_version_tags()` returns a character vector consisting of all
#'   exiting LTO version tags. The \acronym{LTO} versioned release tags are
#'   downloaded from the Theme Ontology GitHub website at
#'   <https://github.com/theme-ontology/theming/releases>.
#'
#' `lto_version_statuses()` prints to console the status (available for
#'   download/cached/defunct) for each \acronym{LTO} version.
#'
#' `configure_lto()` downloads and configures \acronym{LTO} version releases
#'   hosted on the Theme Ontology website at
#'   \url{https://www.themeontology.org/data}.
#'
#' `set_lto()` sets an \acronym{LTO} version as the active version. This means
#'   that package functions will act on this version. The active version is
#'   set to `demo` by default.
#'
#' @template version-arg
#' @param overwrite_json A logical value indicating whether previously
#'   downloaded JSON files (if any) should be re-downloaded and overwritten.
#' @param overwrite_rds A logical value indicating whether previously
#'   generated Rds files (if any) should be regenerated and overwritten.
#' @template verbose-arg
#' @param load_background_collection `r lifecycle::badge("experimental")` A
#'   logical value indicating whether the default background collection of all
#'   LTO thematically annotated stories should be loaded into the stoRy
#'   package environment when activating an LTO version. Setting this to FALSE
#'   may result in some analysis functions not working.
#'
#' @seealso Run [lto-demo] to view the \acronym{LTO} demo data help page.
#' @seealso Run [print_stoRy_cache_info()] to list all cached files.
#'
#' @references Paul Sheridan, Mikael Onsjö, and Janna Hastings, The Literary
#'   Theme Ontology for Media Annotation and Information Retrieval,
#'   Proceedings of JOWO2019: The Joint Ontology Workshops, Graz, Austria,
#'   September 22-26 (\url{https://ceur-ws.org/Vol-2518/paper-WODHSA8.pdf}).
#'
#' @examples \dontrun{
#' # Check which LTO version is active:
#' which_lto()
#'
#' # Print summary info about active LTO version to console:
#' print_lto()
#'
#' # Print summary of existing LTO versions:
#' fetch_lto_version_tags()
#'
#' # Store LTO version tags as a character vector:
#' lto_version_tags <- fetch_lto_version_tags()
#' lto_version_tags
#'
#' # Configure the latest LTO version, only if it is not already setup:
#' configure_lto(version = "latest")
#'
#' # Reconfigure the latest LTO version from scratch:
#' configure_lto(version = "latest", overwrite_json = TRUE, overwrite_rds = TRUE)
#'
#' # Change to latest LTO version:
#' set_lto(version = "latest")
#' }

#' @export
#' @rdname lto
which_lto <- function() stoRy_env$active_version

#' @export
#' @rdname lto
print_lto <- function() {
  version <- which_lto()

  # If `metadata.Rds` is not cached, stop here
  if (!is_lto_file_cached("metadata_tbl.Rds", version)) {
    msg <- get_metadata_tbl_file_not_found_msg(version)
    abort(msg, class = "lto_file_not_found")
  }
  
  # Make sure the correct metadata tibble is used
  if (isTRUE(version == "demo")) {
    metadata_tbl <- metadata_tbl
  } else if (isTRUE(version == "latest")) {
    file_path <- file.path(stoRy_cache_path(),
      get_latest_version_tag(),
      "metadata_tbl.Rds"
    )
    metadata_tbl <- readRDS(file_path)
  } else {
    file_path <- file.path(stoRy_cache_path(), version, "metadata_tbl.Rds")
    metadata_tbl <- readRDS(file_path)
  }

  # Gather LTO version metadata
  timestamp <- metadata_tbl %>%
    filter(.data$name == "timestamp") %>%
    select(.data$value)
  git_commit_id <- metadata_tbl %>%
    filter(.data$name == "git_commit_id") %>%
    select(.data$value)
  encoding <- metadata_tbl %>%
    filter(.data$name == "encoding") %>%
    select(.data$value)
  theme_count <- metadata_tbl %>%
    filter(.data$name == "theme_count") %>%
    select(.data$value)
  story_count <- metadata_tbl %>%
    filter(.data$name == "story_count") %>%
    select(.data$value)
  collection_count <- metadata_tbl %>%
    filter(.data$name == "collection_count") %>%
    select(.data$value)

  # Print LTO version metadata to console
  cli_text("Version: {.val {version}}")
  cli_text("Timestamp: {.val {timestamp}}")
  cli_text("Git Commit ID: {.val {git_commit_id}}")
  cli_text("Encoding: {.val {encoding}}")
  cli_text("Theme Count: {.val {theme_count}}")
  cli_text("Story Count: {.val {story_count}}")
  cli_text("Collection Count: {.val {collection_count}}")

  return(invisible(NULL))
}

#' @export
#' @rdname lto
fetch_lto_version_tags <- function(verbose = TRUE) {
  if (verbose) cli_text("Retrieving LTO version tags...")
  response <- httr::GET(lto_repo_url())
  fetched_versions <- rawToChar(response$content) %>%
                        as.tbl_json %>%
                        gather_array %>%
                        spread_all %>%
                        pull(.data$name)
  downloadable_versions <- c("dev", fetched_versions[!fetched_versions %in% defunct_versions()])
  versions <- c("demo", "dev", fetched_versions)
  versions
}

#' @export
#' @rdname lto
lto_version_statuses <- function(verbose = TRUE) {
  if (verbose) cli_text("Summarizing LTO version info...")
  versions <- fetch_lto_version_tags(verbose)
  
  version <- "demo"
  cli_li("{.val {version}}: A stoRy package included LTO demo version")
  cli_alert_info("Enter {.code ?lto-demo} for more details")
  
  downloadable_versions <- versions[which(versions != "demo")]
  for (version in downloadable_versions) {
    if (are_lto_files_cached(lto_json_file_names(version), version)) {
      cli_li("{.val {version}}: Cached in {.file {file.path(stoRy_cache_path(), version)}}")
      if (!are_newest_lto_json_files_cached(version)) {
        if (isTRUE(version == "dev")) {
          cli_alert_warning("A newly updated developmental version is available for download")
        } else {
          cli_alert_warning("More recently generated JSON files are available for download")
        }
      }
    } else {
      cli_li("{.val {version}}: Available for download")
    }
  }

  for (version in defunct_versions()) {
    cli_li("{.val {version}}: Defunct version")
  }
  cli_end()

  # @PAUL: Add version JSON files to GitHub releases.  
  if (verbose) cli_text("Access LTO version JSON files directly at {.url https://github.com/theme-ontology/theming/releases}")
}


#' @export
#' @rdname lto
configure_lto <- function(
  version,
  verbose = TRUE,
  overwrite_json = FALSE,
  overwrite_rds = FALSE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # If `version` is not a single string, stop here
  if (isTRUE(!is.character(version) || length(version) != 1)) {
    message <- get_single_string_msg(string = version, variable_name = "version")
    abort(message, class = "function_argument_type_check_fail")
  }

  # If it's the "demo" version, stop here
  if (isTRUE(version == "demo")) {
    if (verbose) cli_text("The LTO {.val {version}} version does not require configuration")
    if (verbose) cli_alert_info("Enter {.code ?lto-demo} for more details")
    return(invisible(NULL))
  }

  # If `version` does not corresponds to a valid LTO version tag, stop here
  if (verbose) cli_text("Verifying that {.val {version}} is a valid version tag...")
  if(!is_lto_version_tag_valid(version)) {
    message <- get_invalid_lto_version_msg(version)
    abort(message, class = "lto_version_tag_not_found")
  } else if (verbose) {
    cli_text("Version tag verified") 
  }

  # If all version files are present and not to be overwritten, stop here
  are_json_files_cached <- are_lto_files_cached(lto_json_file_names(version), version)
  are_rds_files_cached <- are_lto_files_cached(lto_rds_file_names(), version)
  if (isTRUE(!overwrite_json && !overwrite_rds && are_json_files_cached && are_rds_files_cached)) {
    if (isTRUE(version %in% c("dev", "latest") && verbose)) {
      cli_text("LTO {.val {version}} version is already configured")
    } else if (verbose) {
      cli_text("LTO {.val {version}} is already configured")
    }
    return(invisible(TRUE))
  }

  # If JSON files are to be downloaded without regenerating rds files, issue warning
  if (isTRUE(overwrite_json && !overwrite_rds)) {
    cli_alert_warning("Overwriting LTO JSON files without regenerating cached Rds files is not recommended")
    cli_alert_info("Run {.code configure_lto(version = \"{version}\", overwrite_json = TRUE, overwrite_rds = TRUE)} to reinstall LTO {.val {version}} from scratch")
  }

  # If `version` is "latest", convert it to the numbered version
  if (isTRUE(version == "latest")) version <- get_latest_version_tag()

  # If LTO JSON files are not cached, download and cached them
  # If `overwrite_json` is TRUE, download and cache the files no matter what
  for (file_name in lto_json_file_names(version)) {
    fetch_lto_file(file_name, verbose, overwrite_json)
  }
  
  # Precompute tibbles for use by stoRy pkg functions and cache as Rds files
  generate_themes_tbl(version, overwrite_rds, verbose)
  generate_stories_tbl(version, overwrite_rds, verbose)
  generate_collections_tbl(version, overwrite_rds, verbose)
  generate_metadata_tbl(version, overwrite_rds, verbose)
  generate_background_collection(version, overwrite_rds, verbose)
  
  if (isTRUE(version == "dev" && verbose)) {
    cli_text("Successfully configured LTO {.val {version}} version!")
  } else if (verbose) {
    cli_text("Successfully configured LTO {.val {version}}!")
  }

  return(invisible(NULL))
}


#' @export
#' @rdname lto
set_lto <- function(
  version,
  verbose = TRUE,
  load_background_collection = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # If `version` is not a single string, stop here
  if (isTRUE(!is.character(version) || length(version) != 1)) {
    msg <- get_single_string_msg(string = version, variable_name = "version")
    abort(msg, class = "function_argument_type_check_fail")
  }

  # If `version` is already the active version, stop here
  if (isTRUE(version == stoRy_env$active_version)) {
    if (verbose) cli_text("LTO {.val {version}} is already the active version")
    return(invisible(NULL))
  }

  # If `version` does not corresponds to a valid LTO version tag, stop here
  if (verbose) cli_text("Verifying that {.val {version}} is a valid version tag...")
  if(!is_lto_version_tag_valid(version)) {
    msg <- get_invalid_lto_version_msg(version)
    abort(msg, class = "lto_version_tag_not_found")
  }

  # If a JSON file is missing, stop here
  if (!are_lto_files_cached(lto_json_file_names(version), version)) {
    msg <- get_lto_json_file_not_found_msg(version)
    abort(msg, class = "lto_json_file_not_found")
  }

  # If an Rds file is missing, stop here
  if (isTRUE(!are_lto_files_cached(lto_rds_file_names(), version) && load_background_collection)) {
    msg <- get_lto_rds_file_not_found_msg(version)
    abort(msg, class = "lto_json_file_not_found")
  }

  # If `version` is "latest", convert it to the numbered version
  if (isTRUE(version == "latest")) version <- get_latest_version_tag()
  
  # Load Rds files into package level environment
  base_path <- file.path(stoRy_cache_path(), version)
  if (verbose) cli_text("Setting {.pkg stoRy} package level environmental variable {.envvar active_version} to {.val {version}}...")
  assign("active_version", version, stoRy_env)
  if (isTRUE(version != "demo")) {
    if (verbose) cli_text("Loading {.file {file.path(base_path, \"collections_tbl.Rds\")}} into {.pkg stoRy} package level environment")
    assign("collections_tbl", readRDS(file.path(base_path, "collections_tbl.Rds")), stoRy_env)
    if (verbose) cli_text("Loading {.file {file.path(base_path, \"stories_tbl.Rds\")}} into {.pkg stoRy} package level environment")
    assign("stories_tbl", readRDS(file.path(base_path, "stories_tbl.Rds")), stoRy_env)
    if (verbose) cli_text("Loading {.file {file.path(base_path, \"themes_tbl.Rds\")}} into {.pkg stoRy} package level environment")
    assign("themes_tbl", readRDS(file.path(base_path, "themes_tbl.Rds")), stoRy_env)
    if (verbose) cli_text("Loading {.file {file.path(base_path, \"metadata_tbl.Rds\")}} into {.pkg stoRy} package level environment")
    assign("metadata_tbl", readRDS(file.path(base_path, "metadata_tbl.Rds")), stoRy_env)
    if (load_background_collection) {
      if (verbose) cli_text("Loading {.file {file.path(base_path, \"background_collection.Rds\")}} into {.pkg stoRy} package level environment")
      assign("background_collection", readRDS(file.path(base_path, "background_collection.Rds")), stoRy_env)
    }
  }
  if (verbose) cli_text("Successfully set active LTO version to {.val {stoRy_env$active_version}}!")

  return(invisible(NULL))
}


#' Download LTO JSON files to cache
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `fetch_lto_file()` downloads \acronym{LTO} version JSON files hosted on
#'   the Theme Ontology website at <https://www.themeontology.org/data>
#'   to cache.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#'
#' @inheritParams lto
#' @examples \dontrun{
#' # Download and cache an LTO developmental version JSON file:
#' fetch_lto_file(file_name = "lto-dev-collections.json", overwrite_json = TRUE)
#' }
#' @export
#' @keywords internal
fetch_lto_file <- function(
  file_name,
  verbose = TRUE,
  overwrite_json = FALSE) {
  # If `file_name` is missing, stop here
  if (is_missing(file_name)) {
    message <- get_missing_arg_msg(variable_name = "file_name")
    abort(message, class = "missing_argument")
  }

  has_lto_file_been_updated <- FALSE
  version <- unlist(strsplit(file_name, split = "-"))[2]
  base_path <- file.path(stoRy_cache_path(), version)
  file_path <- file.path(base_path, file_name)

  # If the file is cached and not to be overwritten, stop here
  if (isTRUE(is_lto_file_cached(file_name, version) && !overwrite_json)) {
    if (verbose) cli_text("The file {.file {file_path}} is cached and will not be downloaded")
    return(invisible(NULL))
  }

  # If already downloaded file is to be overwritten, issue warning
  if (isTRUE(is_lto_file_cached(file_name, version) && overwrite_json && verbose)) {
    cli_alert_warning("The cached file {.file {file_path}} will be overwritten")
  }

  # If the cache folder does not have a version subdirectory, create it
  dir.create(base_path, showWarnings = FALSE, recursive = TRUE)
  
  # Download JSON file from the Theme Ontology website
  file_url <- file.path(base_url(), file_name)
  file_size <- download_size(url = file_url)
  temp_file_path <- tempfile()
  if (verbose) cli_text("Downloading {.file {file_url}}...")
  if (requireNamespace("curl", quietly = TRUE)) {
    if (requireNamespace("progress", quietly = TRUE)) {
      url_payload <- download_url_with_progress_bar(file_url)
      response <- url_payload$response
      contents <- url_payload$contents
    } else {
      cli_alert_warning("{.pkg progress} package not installed, downloading file without an accompanying progress bar")
      response <- curl::curl_fetch_stream(file_url)
    }

    # If download has failed, stop here
    handle_curl_errors(response, file_path)

    # Reformat JSON file before caching, if jsonlite package is installed
    if (verbose) cli_text("Caching {.file {file_path}}... ({formatted_file_size(file_size)})")
    if(requireNamespace("jsonlite", quietly = TRUE)) {
      write(jsonlite::prettify(rawToChar(contents)), temp_file_path)
    } else {
      cli_alert_warning("{.pkg jsonlite} package not installed, falling back to writing unprettified JSON data to file")
      write(rawToChar(contents), temp_file_path)
    }
    file.rename(temp_file_path, file_path)
  } else {
    cli_alert_warning("{.pkg curl} package not installed, falling back to using {.fn download.file}")
    utils::download.file(file_url, file_path)
    if (verbose) cli_text("Cached {.file {file_path}}")
  }

  # Quietly keep track of the JSON file last modified date
  file_path <- file.path(base_path, "lto_file_timestamps.Rds")
  if (!file.exists(file_path)) {    
    lto_file_timestamps_tbl <- tibble(file = lto_json_file_names(version),
                                      timestamp = "Missing")
  } else {
    lto_file_timestamps_tbl <- readRDS(file_path)
  }
  timestamp <- get_website_lto_file_timestamp(file_name)
  lto_file_timestamps_tbl$timestamp[which(lto_json_file_names(version) == file_name)] <- timestamp
  saveRDS(lto_file_timestamps_tbl,
          file = file_path,
          compress = TRUE)
  file_size <- file.info(file_path)$size

  return(invisible(NULL))
}


#' Generate an LTO themes tibble
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `generate_themes_tbl()` generates (and caches) a tibble of all
#'   \acronym{LTO} themes and accompanying metadata.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#' 
#' @inheritParams lto
#' @export
#' @keywords internal
generate_themes_tbl <- function(
  version,
  overwrite_rds = FALSE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # Output file name is hard coded for now
  outfile_name <- "themes_tbl.Rds"
  base_path <- file.path(stoRy_cache_path(), version)
  outfile_path <- file.path(base_path, outfile_name)

  # If themes Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {outfile_name}} is already cached and will not be regenerated")
    return(invisible(NULL))
  }

  # If the LTO JSON file is not cached, stop here
  if (verbose) cli_text("Processing themes...")
  infile_name <- paste0("lto-", version, "-themes.json")
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_lto_json_file_not_found_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }

  # Build a tibble of themes
  json <- read_json(infile_path) %>% as.tbl_json()

  main <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    spread_values(
      theme_name = jstring('name'),
      description = jstring('description'),
      source = jstring('source')
    ) %>%
    select(-.data$document.id) %>%
    as_tibble()

  aliases <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('aliases') %>%
    gather_array %>%
    append_values_string %>%
    rename(aliases = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  notes <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('notes') %>%
    gather_array %>%
    append_values_string %>%
    rename(notes = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  parents <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('parents') %>%
    gather_array %>%
    append_values_string %>%
    rename(parents = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  template <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('template') %>%
    gather_array %>%
    append_values_string %>%
    rename(template = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  examples <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('examples') %>%
    gather_array %>%
    append_values_string %>%
    rename(examples = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  references <- json %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('references') %>%
    gather_array %>%
    append_values_string %>%
    rename(references = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  themes_tbl <- main %>%
    nest_join(notes, by = "theme_index") %>%
    nest_join(aliases, by = "theme_index") %>%
    nest_join(template, by = "theme_index") %>%
    nest_join(parents, by = "theme_index") %>%
    nest_join(examples, by = "theme_index") %>%
    nest_join(references, by = "theme_index") %>%
    relocate(source, .after = "references")

  if (verbose) cli_text("Found {.val {nrow(themes_tbl)}} theme{?/s}")

  # Ancestor theme generation requires some care
  ancestors_lst <- vector(mode = "list", length = nrow(themes_tbl))
  for (i in seq(nrow(themes_tbl))) {
    ancestors <- character(0)
    theme_queue <- themes_tbl %>% `[[`(i, 2)
    while (length(theme_queue) > 0) {
      popped_theme_name <- theme_queue[1] 
      theme_queue <- theme_queue[-1]
      parents <- themes_tbl %>%
        filter(.data$theme_name == !!popped_theme_name) %>%
        pull(parents) %>%
        unlist(use.names = FALSE)
      ancestors <- c(ancestors, parents)
      theme_queue <- c(theme_queue, parents)
    }
    ancestors_lst[[i]] <- as_tibble_col(ancestors, column_name = "ancestors")
  }
  themes_tbl <- themes_tbl %>%
    add_column(ancestors = ancestors_lst, .after = "parents")

  # Cache tibble
  saveRDS(themes_tbl, file = outfile_path, compress = TRUE)
  outfile_size <- file.info(outfile_path)$size
  if (verbose) cli_text("Cached themes tibble to {.file {outfile_path}} ({formatted_file_size(outfile_size)})")

  return(invisible(NULL))
}


#' Generate an LTO stories tibble
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `generate_stories_tbl()` generates (and caches) a tibble of \acronym{LTO}
#'   thematically annotated stories and accompanying metadata.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#' 
#' @inheritParams lto
#' @export
#' @keywords internal
generate_stories_tbl <- function(
  version,
  overwrite_rds = FALSE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # Output file names are hard coded for now...
  base_path <- file.path(stoRy_cache_path(), version)
  stories_outfile_name <- "stories_tbl.Rds"
  stories_outfile_path <- file.path(base_path, stories_outfile_name)
  stub_stories_outfile_name <- "stub_stories_tbl.Rds"
  stub_stories_outfile_path <- file.path(base_path, stub_stories_outfile_name)

  # If stories Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(stories_outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {stories_outfile_name}} is already cached and will not be regenerated")
    return(invisible(NULL))
  }

  # If the LTO stories JSON file is not cached, stop here
  if (verbose) cli_text("Processing stories...")
  infile_name <- paste0("lto-", version, "-stories.json")
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_lto_json_file_not_found_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }

  # Build a tibble of stories
  json <- read_json(infile_path) %>% as.tbl_json()

  main <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    spread_values(
      story_id = jstring('story-id'),
      title = jstring('title'),
      date = jstring('date'),
      description = jstring('description'),
      source = jstring('source')
    ) %>%
    select(-.data$document.id) %>%
    as_tibble()

  component_story_ids <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('component-story-ids') %>%
    gather_array %>%
    append_values_string %>%
    rename(component_story_ids = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  collections <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('collections') %>%
    gather_array %>%
    append_values_string %>%
    rename(collections = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  references <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('references') %>%
    gather_array %>%
    append_values_string %>%
    rename(references = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  theme_names <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('name') %>%
    append_values_string %>%
    rename(theme_name = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_capacities <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('capacity') %>%
    append_values_string %>%
    rename(capacity = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_levels <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('level') %>%
    append_values_string %>%
    rename(level = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_motivations <- json %>%
    enter_object('stories') %>%
    gather_array('story_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('motivation') %>%
    append_values_string %>%
    rename(motivation = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  themes <- theme_names %>%
    right_join(theme_capacities, by = c('story_index', 'theme_index')) %>%
    right_join(theme_levels, by = c('story_index', 'theme_index')) %>%
    right_join(theme_motivations, by = c('story_index', 'theme_index')) %>%
    select(-.data$theme_index) %>%
    as_tibble()

  stories_tbl <- main %>%
    nest_join(component_story_ids, by = "story_index") %>%
    nest_join(collections, by = "story_index") %>%
    nest_join(references, by = "story_index") %>%
    nest_join(themes, by = "story_index") %>%
    relocate(source, .after = "themes")

  # If the themes Rds file does not exist, stop here
  infile_name <- "themes_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  themes_tbl <- readRDS(infile_path)
  all_theme_names <- themes_tbl %>% pull(.data$theme_name)

  # If any undefined or duplicate themes are identified, filter them out
  for (i in seq(nrow(stories_tbl))) {
    story_index <- stories_tbl$story_index[i]
    story_id <- stories_tbl$story_id[story_index]
    themes <- stories_tbl %>%
      filter(.data$story_id == !!story_id) %>%
      select(.data$themes) %>%
      unnest(cols = .data$themes)

    if(nrow(themes) > 0) {
      undefined_theme_names <- stories_tbl %>%
        filter(.data$story_id == !!story_id) %>%
        select(.data$themes) %>% 
        unnest(cols = .data$themes) %>%
        pull(.data$theme_name) %>%
        setdiff(all_theme_names)
      has_undefined_theme <- ifelse(identical(undefined_theme_names, character(0)), FALSE, TRUE)
    
      for (j in seq_along(undefined_theme_names)) {
        themes <- themes %>% filter(.data$theme_name != undefined_theme_names[j])
        if (verbose) cli_text("Dropped undefined theme {.val {undefined_theme_names[j]}} from {.val {story_id}}")
      }

      themes <- themes %>% 
        mutate(themes, theme_index = 1:nrow(themes), .before = .data$theme_name)
      duplicate_themes <- themes %>%
        filter(duplicated(cbind(.data$theme_name, .data$capacity)))
      duplicate_theme_index <- duplicate_themes %>% pull(.data$theme_index)
      duplicate_theme_names <- duplicate_themes %>% pull(.data$theme_name)
      duplicate_theme_capacities <- duplicate_themes %>% pull(.data$capacity)
      has_duplicated_theme <- ifelse(identical(duplicate_theme_names, character(0)), FALSE, TRUE)
    
      for (j in seq_along(duplicate_theme_names)) { 
        if (verbose) {
          if (isTRUE(duplicate_theme_capacities[j] == "")) {
            cli_text("Dropped duplicate theme {.val {duplicate_theme_names[j]}} from {.val {story_id}}")
          } else {
            cli_text("Dropped duplicate theme {.val {duplicate_theme_names[j]}} <{.val {duplicate_theme_capacities[j]}}> from {.val {story_id}}")
          }
          themes <- themes %>% filter(.data$theme_index != duplicate_theme_index[j])
        }
      }

      themes <- themes %>% select(-.data$theme_index)
    
      if (isTRUE(has_undefined_theme || has_duplicated_theme)) {
        stories_tbl$themes[story_index][[1]] <- themes %>%
          distinct(.data$theme_name, .keep_all = TRUE)
      }
    }
  }

  # Split off stub stories into their own tibble
  # Note: A frame story is only considered to be a stub when it
  # 1) has no themes, and
  # 2) none of its component stories have any themes.
  
  # If Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(stub_stories_outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {stub_stories_outfile_name}} is already cached and will not be regenerated")
  } else {
    # Initialize vector of stub story indexes
    stub_story_indices <- NULL

    for (i in seq(nrow(stories_tbl))) {
      story_index <- stories_tbl$story_index[i]
      story_id <- stories_tbl$story_id[story_index]
      component_story_ids <- stories_tbl %>%
        filter(.data$story_id == !!story_id) %>%
        select(.data$component_story_ids) %>%
        unlist(use.names = FALSE)
      number_of_component_stories <- length(component_story_ids)
      number_of_themes <- stories_tbl %>%
        filter(.data$story_id == !!story_id) %>%
        select(.data$themes) %>%
        unnest(cols = .data$themes) %>%
        nrow()

      # Count up the number of themes in component stories
      total_number_of_component_story_themes <- 0
      for (component_story_id in component_story_ids) {
        number_of_component_story_themes <- stories_tbl %>%
          filter(.data$story_id == !!component_story_id) %>%
          select(.data$themes) %>%
          unnest(cols = .data$themes) %>%
          nrow()
        total_number_of_component_story_themes <- total_number_of_component_story_themes + length(number_of_component_story_themes)
      }

      if (isTRUE(number_of_themes == 0 && total_number_of_component_story_themes == 0)) {
        stub_story_indices <- c(stub_story_indices, story_index)
      }
    }

    # Create stub story ids tibble
    if (isTRUE(length(stub_story_indices) > 0)) {
      stub_stories_tbl <- stories_tbl[stub_story_indices, ]
    } else {
      stub_stories_tbl <- stories_tbl[-stories_tbl$story_index, ]
    }
  }

  if (verbose) cli_text("Found {.val {nrow(stories_tbl)}} stor{?y/ies} of which {.val {nrow(stub_stories_tbl)}} {?is/are} stub{?/s}")

  # If any stub stories were found, filter them out
  if (isTRUE(nrow(stub_stories_tbl) > 0)) {
    #stories_tbl <- stories_tbl %>% filter(!.data$story_id %in% .data$stub_story_ids_tbl$story_id)
    stories_tbl <- stories_tbl[-stub_story_indices, ]

    # Redefine story indices accordingly
    stories_tbl$story_index <- 1 : nrow(stories_tbl)
    stub_stories_tbl$story_index <- (nrow(stories_tbl) + 1) : (nrow(stub_stories_tbl) + nrow(stories_tbl))
  }

  # Cache stories tibble
  saveRDS(stories_tbl, file = stories_outfile_path, compress = TRUE)
  stories_outfile_size <- file.info(stories_outfile_path)$size
  if (verbose) cli_text("Cached stories tibble to {.file {stories_outfile_path}} ({formatted_file_size(stories_outfile_size)})")

  # Cache stub stories tibble
  saveRDS(stub_stories_tbl, file = stub_stories_outfile_path, compress = TRUE)
  stub_stories_outfile_size <- file.info(stub_stories_outfile_path)$size
  if (verbose) cli_text("Cached stub stories tibble to {.file {stub_stories_outfile_path}} ({formatted_file_size(stub_stories_outfile_size)})")

  return(invisible(NULL))
}


#' Generate an LTO collections tibble
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `generate_collections_tbl()` generates (and caches) a tibble of
#'   \acronym{LTO} thematically annotated collections and accompanying
#'   metadata.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#' 
#' @inheritParams lto
#' @export
#' @keywords internal
#' @importFrom stringr str_c
generate_collections_tbl <- function(
  version,
  overwrite_rds = FALSE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # Output file names are hardcoded for now...
  base_path <- file.path(stoRy_cache_path(), version)
  collections_outfile_name <- "collections_tbl.Rds"
  collections_outfile_path <- file.path(base_path, collections_outfile_name)
  stub_collections_outfile_name <- "stub_collections_tbl.Rds"
  stub_collections_outfile_path <- file.path(base_path, stub_collections_outfile_name)

  # If collections Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(collections_outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {collections_outfile_name}} is already cached and will not be regenerated")
    return(invisible(NULL))
  }

  # If the LTO collections JSON file is not cached, stop here
  if (verbose) cli_text("Processing collections...")
  infile_name <- paste0("lto-", version, "-collections.json")
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_lto_json_file_not_found_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }

  # Build a tibble of collections
  json <- read_json(infile_path) %>% as.tbl_json()

  main <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    spread_values(
      collection_id = jstring('collection-id'),
      title = jstring('title'),
      date = jstring('date'),
      description = jstring('description'),
      source = jstring('source')
    ) %>%
    select(-.data$document.id) %>%
    as_tibble()

  component_story_ids <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('component-story-ids') %>%
    gather_array %>%
    append_values_string %>%
    rename(component_story_ids = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  references <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('references') %>%
    gather_array %>%
    append_values_string %>%
    rename(references = string) %>%
    select(-.data$document.id, -.data$array.index) %>%
    as_tibble()

  theme_names <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('name') %>%
    append_values_string %>%
    rename(theme_name = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_capacities <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('capacity') %>%
    append_values_string %>%
    rename(capacity = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_levels <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('level') %>%
    append_values_string %>%
    rename(level = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  theme_motivations <- json %>%
    enter_object('collections') %>%
    gather_array('collection_index') %>%
    enter_object('themes') %>%
    gather_array('theme_index') %>%
    enter_object('motivation') %>%
    append_values_string %>%
    rename(motivation = string) %>%
    select(-.data$document.id) %>%
    as_tibble()

  themes <- theme_names %>%
    right_join(theme_capacities, by = c('collection_index', 'theme_index')) %>%
    right_join(theme_levels, by = c('collection_index', 'theme_index')) %>%
    right_join(theme_motivations, by = c('collection_index', 'theme_index')) %>%
    select(-.data$theme_index) %>%
    as_tibble()

  collections_tbl <- main %>%
    nest_join(component_story_ids, by = "collection_index") %>%
    nest_join(references, by = "collection_index") %>%
    nest_join(themes, by = "collection_index") %>%
    relocate(source, .after = "themes")

  # If the themes Rds file does not exist, stop here
  infile_name <- "themes_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  themes_tbl <- readRDS(infile_path)
  all_theme_names <- themes_tbl %>% pull(.data$theme_name)

  # If any undefined or multiply occurring themes are identified, filter them out
  for (i in seq(nrow(collections_tbl))) {
    collection_index <- collections_tbl$collection_index[i]
    collection_id <- collections_tbl$collection_id[collection_index]
    themes <- collections_tbl %>%
      filter(.data$collection_id == !!collection_id) %>%
      select(.data$themes) %>%
      unnest(cols = .data$themes)

    has_undefined_theme <- FALSE
    undefined_theme_names <- collections_tbl %>%
      filter(.data$collection_id == !!collection_id) %>%
      select(.data$themes) %>% 
      unnest(cols = .data$themes) %>%
      pull(.data$theme_name) %>%
      setdiff(all_theme_names)

    for (j in seq_along(undefined_theme_names)) {
      has_undefined_theme <- TRUE
      themes <- themes %>%
        filter(.data$theme_name != undefined_theme_names[j])
      if (verbose) cli_text("Dropped undefined theme {.val {undefined_theme_names[j]}} from {.val {collection_id}}")
    }

    duplicate_theme_name_tokens <- themes %>%
      filter(duplicated(.data$theme_name)) %>%
      pull(.data$theme_name)
    duplicate_theme_counts <- table(duplicate_theme_name_tokens)
    duplicate_theme_names <- duplicate_theme_name_tokens %>% unique()
    has_duplicated_theme <- ifelse(identical(duplicate_theme_names, character(0)), FALSE, TRUE)
    for (j in seq_along(duplicate_theme_names)) {
      duplicate_theme_count <- as.numeric(duplicate_theme_counts[duplicate_theme_names[j]])
      if (isTRUE(duplicate_theme_count > 1 && verbose)) {
        cli_text("Dropped {.val {duplicate_theme_count - 1}} of {.val {duplicate_theme_count}} occurrences of {.val {duplicate_theme_names[j]}} from {.val {story_id}}")
      }
    }
    
    if (isTRUE(has_undefined_theme || has_duplicated_theme)) {
      stories_tbl$themes[.data$story_index][[1]] <- themes %>%
        distinct(.data$theme_name, .keep_all = TRUE)
    }
  }

  # If the stub stories Rds file is not found, stop here
  infile_name <- "stub_stories_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  stub_stories_tbl <- readRDS(infile_path)

  # If any stub collections are identified, excise them
  stub_collection_indices <- NULL
  candidate_stub_collection_ids <- intersect(collections_tbl$collection_id, str_c("Collection: ", stub_stories_tbl$story_id))
  for (i in seq(nrow(collections_tbl))) {
    collection_index <- collections_tbl$collection_index[i]
    collection_id <- collections_tbl$collection_id[collection_index]
    
    if (isTRUE(collection_id %in% candidate_stub_collection_ids)) {
      component_story_ids <- collections_tbl %>%
        filter(.data$collection_id == !!collection_id) %>%
        select(.data$component_story_ids) %>%
        unlist(use.names = FALSE)
      nonstub_component_story_ids <- setdiff(component_story_ids, stub_stories_tbl$story_id)
      
      if (isTRUE(length(nonstub_component_story_ids) == 0)) {
        stub_collection_indices <- c(stub_collection_indices, collection_index)
      }
    }
  }

  # Initialize stub collections tibble and make updates accordingly
  if (isTRUE(length(stub_collection_indices) > 0)) {
    stub_collections_tbl <- collections_tbl[stub_collection_indices, ]
  } else {
    stub_collections_tbl <- collections_tbl[-collections_tbl$collection_index, ]
  }

  # If any stub stories are found in collections, excise them
  if (isTRUE(nrow(collections_tbl) > 0)) {
    for (i in seq(nrow(collections_tbl))) {
      collection_index <- collections_tbl$collection_index[i]
      collection_id <- collections_tbl$collection_id[collection_index]
      component_story_ids <- collections_tbl %>%
        filter(.data$collection_id == !!collection_id) %>%
        pull(.data$component_story_ids) %>%
        unlist(use.names = FALSE)
      nonstub_component_story_ids <- setdiff(component_story_ids , stub_stories_tbl$story_id)
      number_of_stub_component_story_ids <- length(component_story_ids) - length(nonstub_component_story_ids)

      if (isTRUE(number_of_stub_component_story_ids > 0)) {
        if (verbose) cli_text("Dropped {.val {number_of_stub_component_story_ids}} stub component stor{?y/ies} from {.val {collection_id}}")
        collections_tbl$component_story_ids[collection_index][[1]] <- tibble(component_story_ids = nonstub_component_story_ids)
      }
    }
  }

  # If any stub collections were found, excise them and update indices accordingly
  if (isTRUE(length(stub_collection_indices) > 0)) {
    collections_tbl <- collections_tbl[-stub_collection_indices, ]
    collections_tbl$collection_index <- 1 : nrow(collections_tbl)
    stub_collections_tbl$collection_index <- (nrow(collections_tbl) + 1) : (nrow(stub_collections_tbl) + nrow(collections_tbl))
  }

  if (verbose) cli_text("Found {.val {nrow(collections_tbl)}} collection{?/s} of which {.val {nrow(stub_collections_tbl)}} {?is a/are} stub{?/s}")
   
  # If the stories Rds file is not found, stop here
  infile_name <- "stories_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  stories_tbl <- readRDS(infile_path)

  # Initialize collection tibble
  collections_tbl <- collections_tbl %>% add_row(tibble(
    collection_index = NA,
    collection_id = "Collection: All Stories",
    title = "All Stories",
    date = NA,
    description = "All LTO thematically annotated stories.",
    component_story_ids = list(tibble(component_story_ids = stories_tbl$story_id)),
    references = list(tibble(references = character(0))),
    themes = list(tibble(data.frame(theme_name = character(0), level = character(0), motivation = character(0)))),
    source = NA))
  collections_tbl$collection_index <- 1 : nrow(collections_tbl)

  # Cache collections tibble
  saveRDS(collections_tbl, file = collections_outfile_path, compress = TRUE)
  collections_outfile_size <- file.info(collections_outfile_path)$size
  if (verbose) cli_text("Cached collections tibble to {.file {collections_outfile_path}} ({formatted_file_size(collections_outfile_size)})")

  # Cache stub collections tibble
  saveRDS(stub_collections_tbl, file = stub_collections_outfile_path, compress = TRUE)
  stub_collections_outfile_size <- file.info(stub_collections_outfile_path)$size
  if (verbose) cli_text("Cached stub collections tibble to {.file {stub_collections_outfile_path}} ({formatted_file_size(stub_collections_outfile_size)})")

  return(invisible(NULL))
}

#' Generate an LTO metadata tibble
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `generate_metadata_tbl()` generates (and caches) a tibble of \acronym{LTO}
#'   metadata.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#' 
#' @inheritParams lto
#' @export
#' @keywords internal
generate_metadata_tbl <- function(
  version,
  overwrite_rds = FALSE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }

  # Output file name is hardcoded for now...
  outfile_name <- "metadata_tbl.Rds"
  base_path <- file.path(stoRy_cache_path(), version)
  outfile_path <- file.path(base_path, outfile_name)

  # If Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {outfile_name}} is already cached and will not be regenerated")
    return(invisible(NULL))
  }

  if (verbose) cli_text("Processing metadata...")

  metadata_tbl <- tibble(
    name = character(),
    value = character()
  )
  
  # If the LTO themes JSON file is not cached, stop here
  infile_name <- paste0("lto-", version, "-themes.json")
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_lto_json_file_not_found_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }

  json <- read_json(infile_path) %>% as.tbl_json()

  # Retrieve basic LTO metadata
  themes_metadata_tbl <- json %>%
    enter_object('lto') %>%
    gather_object %>%
    append_values_string
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "version", value = themes_metadata_tbl %>% `[[`(1, 3))
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "timestamp", value = themes_metadata_tbl %>% `[[`(2, 3))
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "git_commit_id", value = themes_metadata_tbl %>% `[[`(3, 3))
  metadata_tbl <- metadata_tbl %>%
   add_row(name = "encoding", value = themes_metadata_tbl %>% `[[`(4, 3))
  
  # If the themes Rds file does not exist, stop here
  infile_name <- "themes_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  themes_tbl <- readRDS(infile_path)

  # Retrieve number of themes metadatum
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "theme_count", value = as.character(nrow(themes_tbl)))

  # If the LTO stories Rds file is not cached, stop here
  infile_name <- "stories_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_lto_json_file_not_found_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  stories_tbl <- readRDS(infile_path)

  # Retrieve number of stories metadatum
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "story_count", value = as.character(nrow(stories_tbl)))
  
  # If the collections Rds file does not exist, stop here
  infile_name <- "collections_tbl.Rds"
  infile_path <- file.path(base_path, infile_name)
  if (!file.exists(infile_path)) {
    message <- get_missing_lto_rds_file_msg(version, infile_path)
    abort(message, class = "file_not_found")
  }
  collections_tbl <- readRDS(infile_path)

  # Retrieve number of collections metadatum
  metadata_tbl <- metadata_tbl %>%
    add_row(name = "collection_count", value = as.character(nrow(collections_tbl)))

  # Cache tibble
  saveRDS(metadata_tbl, file = outfile_path, compress = TRUE)
  outfile_size <- file.info(outfile_path)$size
  if (verbose) cli_text("Cached metadata tibble to {.file {outfile_path}} ({formatted_file_size(outfile_size)})")

  return(invisible(NULL))
}


#' Generate default background collection
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' `generate_background_collection()` generates (and caches) a `Collection`
#'   objecting consisting of all non-stub \acronym{LTO} thematically annotated
#'   stories.
#'
#' @details This function is called internally by the `configure_lto()`
#'   package function. Use it directly at your own risk.
#' 
#' @inheritParams lto
#' @export
#' @keywords internal
generate_background_collection <- function(
  version,
  overwrite_rds = FALSE,
  verbose = TRUE) {
  # If `version` is missing, stop here
  if (is_missing(version)) {
    message <- get_missing_arg_msg(variable_name = "version")
    abort(message, class = "missing_argument")
  }
  
  # Output file name is hardcoded for now...
  outfile_name <- "background_collection.Rds"
  base_path <- file.path(stoRy_cache_path(), version)
  outfile_path <- file.path(base_path, outfile_name) 
  
  # If Rds file exists and is not to be overwritten, stop here
  if (isTRUE(file.exists(outfile_path) && !overwrite_rds && verbose)) {
    cli_text("The file {.file {outfile_name}} is already cached and will not be regenerated")
    return(invisible(NULL))
  }

  if (verbose) cli_text("Generating default background collection...")

  # Need to temporarily make `version` the active version
  old_active_version <- which_lto()
  set_lto(version, verbose = FALSE, load_background_collection = FALSE)
  collections_tbl <- get_collections_tbl() %>%
    filter(.data$collection_id == "Collection: All Stories")
  background_collection <- Collection$new(collection_id = "Collection: All Stories")
  set_lto(version = old_active_version, verbose = FALSE)

  # Cache tibble
  saveRDS(background_collection, file = outfile_path, compress = TRUE)
  outfile_size <- file.info(outfile_path)$size
  if (verbose) cli_text("Cached default background collection to {.file {outfile_path}} ({formatted_file_size(outfile_size)})")

  return(invisible(NULL))
}


