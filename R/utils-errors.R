# Non-export helper functions

get_missing_arg_msg <- function(variable_name) {
  str_glue("`{variable_name}` is missing with no default.")
}

get_single_string_msg <- function(string, variable_name) {
  error <- "`{variable_name}` must be a single string.\n"
  info <- NULL
  if (isTRUE(variable_name == "version")) {
    info <- "{col_yellow(symbol$info)} Run 'fetch_lto_version_tags()' to list valid LTO versions."
  }

  if (is.null(info)) {
    message <- str_glue(error)
  } else {
    message <- str_glue(
      error,
      info
    )
  }

  message
}

get_file_not_found_msg <- function(file) {
  str_glue(
    "The `file` specified file does not exist.\n",
    "{col_red(symbol$cross)} '{file}' was not found."
  )
}

get_empty_file_msg <- function(file) {
  str_glue(
    "The `file` specified file is empty.\n",
    "{col_red(symbol$cross)} '{file}' must be nonempty."
  )
}

get_invalid_lto_id_msg <- function(id_class) {
  error <- "`{id_class}` does not exist.\n"
  cross <- "{col_red(symbol$cross)} You've supplied a {str_split(id_class, \"_\", simplify = TRUE)[1]} ID that is not associated with LTO version {stoRy_env$active_version}.\n"
  info <- NULL
  if (isTRUE(id_class == "story_id")) { 
    info <- "{col_yellow(symbol$info)} Run 'clone_active_stories_tbl() %>% select(story_id)' to get a tibble of all LTO {stoRy_env$active_version} version story IDs."
  } else if (isTRUE(id_class == "collection_id")) {
    info <- "{col_yellow(symbol$info)} Run 'clone_active_collections_tbl() %>% select(collection_id)' to get a tibble of all LTO {stoRy_env$active_version} version collection IDs."
  }

  if (is.null(info)) {
    message <- str_glue(
      error,
      cross
    )
  } else {
    message <- str_glue(
      error,
      cross,
      info
    )
  }

  message
}

get_invalid_file_extension_msg <- function(file_name, valid_file_extension) {
  error <- "The `file` specified file name is invalid.\n"
  cross <- "{col_red(symbol$cross)} '{file_name}' has an invalid extension.\n"
  info <- NULL
  if (isTRUE(valid_file_extension == ".st.txt")) {
    info <- "{col_yellow(symbol$info)} Collection files must use the .st.txt file extension."
  } else if (isTRUE(valid_file_extension == ".thset.txt")) {
    info <- "{col_yellow(symbol$info)} Themeset files must use the .thset.txt file extension."
  }

  if (is.null(info)) {
    message <- str_glue(
      error,
      cross
    )
  } else {
    message <- str_glue(
      error,
      cross,
      info
    )
  }

  message
}

get_invalid_collection_msg <- function(collection_name, null_ok = FALSE) {
  error <- "`{collection_name}` is invalid.\n"
  if (null_ok) {
    cross <- "{col_red(symbol$cross)} The `{collection_name}` input must be either a Collection class object or else `NULL`.\n"  
  } else {
    cross <- "{col_red(symbol$cross)} The `{collection_name}` input must be a Collection class object.\n"
  }
  info <- "{col_yellow(symbol$info)} Run `?Collection` for more info on Collection class objects."

  str_glue(
    error,
    cross,
    info
  )
}

get_invalid_story_msg <- function(story_name) {
  str_glue(
    "`{story_name}` is invalid.\n",
    "{col_red(symbol$cross)} The `{story_name}` input must be either a Story class object`.\n",
    "{col_yellow(symbol$info)} Run `?Story` for more info on Story class objects."
  )
}

get_not_positive_integer_msg <- function(variable, infinity = FALSE) {
  error <- "`{variable}` is invalid.\n"
  if (infinity) {
    cross <- "{col_red(symbol$cross)} The `{variable}` input must be either a positive integer or `Inf`."
  } else {
    cross <- "{col_red(symbol$cross)} The `{variable}` input must be a positive integer."
  }

  message <- str_glue(
    error,
    cross
  )

  message
}

get_invalid_element_msg <- function(variable, choices) {
  str_glue(
    "`{variable}` is invalid.\n",
    "{col_red(symbol$cross)} The `{variable}` input must be a subset of `{choices}`."
  )
}

check_theme_level_weights <- function(weights) {
  if (!is_list(weights)) {
    message <- get_not_list_msg(variable = "weights")
    abort(message, class = "class_check_fail")
  } else if (isTRUE(length(weights) != 3)) {
    message <- get_invalid_number_of_weights_msg()
    abort(message, class = "function_argument_type_check_fail")
  } else if (!identical(names(weights), theme_levels())) {
    message <- get_invalid_weight_names_msg()
    abort(message, class = "function_argument_type_check_fail")
  } else if (isFALSE(weights$choice >= 0 && weights$major >= 0 && weights$minor >= 0)) {
    message <- get_invalid_weight_values_msg()
    abort(message, class = "function_argument_type_check_fail")
  } else if (isTRUE(weights$choice == 0 && weights$major == 0 && weights$minor == 0)) {
    message <- get_zero_weights_msg()
    abort(message, class = "function_argument_type_check_fail")
  }

  return(invisible(NULL))
}

get_not_list_msg <- function(variable) {
  str_glue(
    "`{variable}` is invalid.\n",
    "{col_red(symbol$cross)} The `{variable}` input must be a list."
  )
}

get_invalid_number_of_weights_msg <- function() {
  str_glue(
    "`weights` is invalid.\n",
    "{col_red(symbol$cross)} The `weights` input must be a list with three entries."
  )
}

get_invalid_weight_names_msg <- function() {
  str_glue(
    "`weights` is invalid.\n",
    "{col_red(symbol$cross)} The `weights` input must be a list with three entries named \"choice\", \"major\", and \"minor\" in that order."
  )
}

get_invalid_weight_values_msg <- function() {
  str_glue(
    "`weights` is invalid.\n",
    "{col_red(symbol$cross)} The `weights` input must contain nonnegative values."
  )
}

get_zero_weights_msg <- function() {
  str_glue(
    "`weights` is invalid.\n",
    "{col_red(symbol$cross)} The `weights` input values are all zero.\n",
    "{col_yellow(symbol$info)} At least one of the `weights` input values must be positive."
  )
}

get_not_logical_msg <- function(variable) {
  str_glue(
    "`{variable}` is invalid.\n",
    "{col_red(symbol$cross)} The `{variable}` input must be one of `TRUE` or `FALSE`."
  )
}

get_invalid_lto_version_msg <- function(version) {
  str_glue("`version` must correspond to a valid LTO version tag.\n",
    "{col_red(symbol$cross)} '{version}' is not associated with an LTO version.\n",
    "{col_yellow(symbol$info)} Run 'fetch_lto_version_tags()' to list valid LTO versions."
  )
}

get_invalid_themeset_msg <- function(variable) {
  str_glue(
    "`{variable}` specified LTO file type is invalid.\n",
    "{col_red(symbol$cross)} The `{variable}` input must be either a Themeset class object or else `NULL`.\n",
    "{col_yellow(symbol$info)} Run `?Themeset` for more info on Themeset class objects."
  )
}

get_lto_json_file_not_found_msg <- function(version, file = NULL) {
  if (is.null(file)) {
    if (isTRUE(version == "dev" || version == "latest")) {
      error <- "At least one LTO '{version}' version JSON file is missing.\n"
    } else {
      error <- "At least one LTO '{version}' JSON file is missing.\n"
    }

    return(str_glue(error,
      "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\" overwrite_json = TRUE, overwrite_rds = TRUE)' to download the missing file(s) and setup this LTO version."
    ))
  }
  
  str_glue("An LTO JSON file is missing.\n",
    "{col_red(symbol$cross)} The file '{file}' does not exist.\n",
    "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite_json = TRUE, overwrite_rds = TRUE)' to download '{file}' and setup this LTO version."
  )
}

get_lto_rds_file_not_found_msg <- function(version, file = NULL) {
  if (is.null(file)) {
    if (isTRUE(version == "dev" || version == "latest")) {
      error <- "At least one LTO '{version}' version Rds file is missing.\n"
    } else {
      error <- "At least one LTO '{version}' Rds file is missing.\n"
    }

    return(str_glue(error,
      "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite_rds = TRUE)' to regenerate the missing file(s)."
    ))
  }
  
  str_glue("An LTO JSON file is missing.\n",
    "{col_red(symbol$cross)} The file '{file}' does not exist.\n",
    "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite_rds = TRUE)' to regenerate '{file}'."
  )
}

get_metadata_tbl_file_not_found_msg <- function(version) {
  if (isTRUE(version == "dev" || version == "latest")) {
    error <- "The LTO '{version}' version metadata_tbl.Rds file is missing.\n"
  } else {
    error <- "The LTO '{version}' metadata_tbl.Rds file is missing.\n"
  }

  str_glue(error,
    "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite_rds = TRUE)' to generate the missing file and setup this LTO version."
  )
}

get_missing_lto_rds_file_msg <- function(version, file) {
  str_glue("A precomputed Rds file is missing.\n",
    "{col_red(symbol$cross)} {.file {file}} does not exist.\n",
    "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite = TRUE)' to regenerate the missing file."
  )
}

get_lto_not_configured_msg <- function(version) {
  str_glue("Lto.\n",
    "{col_red(symbol$cross)} {.file {file}} does not exist.\n",
    "{col_yellow(symbol$info)} Run 'configure_lto(version = \"{version}\", overwrite = TRUE)' to regenerate the missing file."
  )
}
