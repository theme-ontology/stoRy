# Non-export helper functions used for parsing LTO format files

# The header line of an LTO file must start with one of these fragments
get_lto_file_info_tbl <- function() {
  lto_file_info_tbl <- tibble(
    type = c("collection", "themeset"),
    extension = c(".st.txt", ".thset.txt"),
    header_fragment = c("Collection:", "Themeset:")
  )

  lto_file_info_tbl
}

# Convert an LTO file to a vector of strings
lto_file_to_lines <- function(file, type) {
  # If `file` is missing, stop here
  if (is_missing(file)) {
    message <- get_missing_arg_msg(variable_name = "file")
    abort(message, class = "missing_argument")
  }

  # If `file` is not a single string, stop here
  if (length(file) != 1 || !is.character(file)) {
    message <- get_single_string_msg(string = file, variable_name = type)
    abort(message, class = "function_argument_type_check_fail")
  }

  # If `type` is invalid, stop here
  if (isTRUE(!(type %in% get_lto_file_info_tbl()$type))) {
    abort(
      str_glue(
        "The `type` specified LTO file type is invalid.\n",
        "{col_red(symbol$cross)} {type} is an invalid LTO file type.\n",
        "{col_yellow(symbol$info)} Valid LTO file types are:\n",
        "{str_c(get_lto_file_info_tbl()$type, collapse = \"\n\")}"
      )
    )
  }
  
  # If `file` is detected to be a file name or file path, do some checks
  if (isTRUE(!is_url(file) && !grepl("\n", file))) {
    if (is_absolute_path(file)) {
      file_name <- basename(file)
      file_path <- file
    } else {
      file_name <- file
      file_path <- file.path(getwd(), file)
    }

    # If the `file` file extension is invalid, stop here
    if (all(str_ends(file, get_lto_file_info_tbl()$extension, negate = TRUE))) {
      if (isTRUE(type == "collection")) {
        extension <- get_lto_file_info_tbl() %>%
          filter(.data$type == "collection") %>%
          pull(.data$extension)
      } else if (isTRUE(type == "themeset")) {
        extension <- get_lto_file_info_tbl() %>%
          filter(.data$type == "themeset") %>%
          pull(.data$extension)
      }
      cli_text("file_name: {.val {file}}")
      cli_text("extension: {.val {extension}}")
      message <- get_invalid_file_extension_msg(file_name = file, valid_file_extension = extension)
      abort(message, class = "invalid_file_extension")
    }

    # If `file` does not exist, stop here
    if (!file.exists(file)) {
      message <- get_file_not_found_msg(file)
      abort(message, class = "file_not_found")
    }

    # If `file` is empty, stop here
    if (is_file_empty(file)) {
      message <- get_empty_file_msg(file)
      abort(message, class = "file_empty")
    }
  }

  # Read and return parsed `file` contents
  lines <- readr::read_lines(file)
  lines
}

# Parse an LTO file into tibble format
# The file is assumed to be already read in as a character vector of strings
lto_file_to_tbl <- function(lines, verbose = TRUE) {
  # If `lines` is missing, stop here
  if (is_missing(lines)) {
    message <- get_missing_arg_msg(variable_name = "lines")
    abort(message, class = "missing_argument")
  }

  # Hard coded info messages
  info1 <- "{col_yellow(symbol$info)} Run `cat(readr::read_file(system.file(\"extdata\", \"rolling-stone-best-ttz1959-episodes.st.txt\", package = \"stoRy\")))` to view an example collection file.\n"
  info2 <- "{col_yellow(symbol$info)} Run `cat(readr::read_file(system.file(\"extdata\", \"immortality.thset.txt\", package = \"stoRy\")))` to view an example themeset file."

  # Read in some hard coded information about LTO fiel formats
  lto_file_types_tbl <- get_lto_file_info_tbl()
  field_def_tbl <- get_field_def_tbl()

  # Initialize the tibble where the parsed info from `lines` is stored
  parsed_fields_tbl <- tibble(
    name = character(0),
    contents = list(tibble(contents = character(0))))

  # End of file hack 
  if (last(lines) != "") lines <- append(lines, "")

  # If the second line of `file` doesn't begin with "===", stop here
  if (!str_starts(lines[2], pattern = "===")) {
    abort(
      str_glue(
        "`file` contains an invalid header.\n",
        "{col_red(symbol$cross)} The second line must start with \"===\".\n",
        info1,
        info2
      ),
      class = "invalid_lto_file_format"
    )
  }

  # Read header line
  header <- lines[1]
  
  # If the LTO header has an invalid prefix, stop here
  if (!any(str_starts(header, pattern = lto_file_types_tbl$header_fragment))) {
    abort(
      str_glue(
        "`file` contains an invalid header.\n",
        "{col_red(symbol$cross)} The first line must start with one of the prefixes\n",
        "{str_c(lto_file_types_tbl$header_fragment, collapse = \"\n)}\"\n",
        info1,
        info2
      ),
      class = "invalid_lto_file_format"
    )
  }

  # If the header line contains a comma, stop here
  if (str_detect(header, ",")) {
    abort(
      str_glue(
        "`file` contains an invalid header.\n",
        "{col_red(symbol$cross)} The first line must not contain any \",\" characters.\n",
        info1,
        info2
      ),
      class = "invalid_lto_file_format"
    )
  }

  # Store the header line
  parsed_fields_tbl <-  parsed_fields_tbl %>%
    add_row(tibble(name = "header", contents = list(tibble(contents = header))))
  
  # Identify field start/end lines
  field_line_numbers <- str_which(lines, "^::")
  field_content_start_line_numbers <- field_line_numbers + 1
  field_content_end_line_numbers <- append(field_line_numbers[-1] - 1, length(lines))
  number_of_fields <- length(field_line_numbers)

  # Parse fields and organize as tibble
  for (field_number in seq_len(number_of_fields)) {
    field_literal <- lines[field_line_numbers[field_number]]

    if (!isTRUE(field_literal %in% field_def_tbl$literal)) {
      if (verbose) cli_alert_warning("Discarding unrecognized field: \"{field_literal}\"")
    } else if (isTRUE(field_def_tbl %>% filter(.data$literal == field_literal) %>% pull(.data$status) == "unsupported")) {
      if (verbose) cli_alert_warning("Discarding unsupported field: \"{field_literal}\"")
    } else {
      name <- field_def_tbl %>%
        filter(.data$literal == field_literal) %>%
        pull(.data$name)
      text <- str_c(lines[field_content_start_line_numbers[field_number] : field_content_end_line_numbers[field_number]], collapse = "\n")
      format <- field_def_tbl %>%
        filter(.data$literal == field_literal) %>%
        pull(.data$format)
      contents <- parse_field_contents(text, format, field_literal)
      parsed_fields_tbl <- parsed_fields_tbl %>%
        add_row(tibble(name = name, contents = list(contents)))
    }
  }

  # Find out the file type
  header <- parsed_fields_tbl %>% 
    filter(.data$name == "header") %>%
    pull(.data$contents) %>%
    unlist(use.names = FALSE)
  lto_file_type <- get_lto_file_type(string = header)

  # If the "Description" field is missing, stop here
  expected_field_name <- "description"
  if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
    abort(
      str_glue(
        "`file` is missing a required field.\n",
        "{col_red(symbol$cross)} The required field \":: Description\" is missing.\n",
        info1,
        info2
      ),
      class = "invalid_lto_file_format"
    )
  }

  # Parse fields conditional on the LTO file type
  if (isTRUE(lto_file_type == "themeset")) {
    # If the "Component Themes" field is missing, stop here
    expected_field_name <- "component_theme_names"
    if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
      abort(
        str_glue(
          "`file` is missing a required field.\n",
          "{col_red(symbol$cross)} The required field \":: Component Themes\" is missing.\n",
          info2
        ),
        class = "invalid_lto_file_format"
      )
    }

    # Parse component themes, storing valid ones
    valid_theme_names <- get_themes_tbl() %>% pull(.data$theme_name)
    candidate_component_theme_names <- parsed_fields_tbl %>%
      filter(.data$name == expected_field_name) %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    invalid_component_theme_names <- setdiff(candidate_component_theme_names, valid_theme_names)
    if (!identical(invalid_component_theme_names, character(0))) {
      if (verbose) cli_alert_warning("Discarding invalid themes:\n", "{str_c(invalid_component_theme_names, collapse = \"\n\")}")
      component_theme_names <- intersect(candidate_component_theme_names, valid_theme_names)
      parsed_fields_tbl$contents[[which(parsed_fields_tbl$name == expected_field_name)]] <- tibble(contents = component_theme_names)
    }
  } else if (isTRUE(lto_file_type == "collection")) {
    # If the "Title" field is missing, stop here
    expected_field_name <- "title"
    if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
      abort(
        str_glue(
          "`file` is missing a required field.\n",
          "{col_red(symbol$cross)} The required field \":: Title\" is missing.\n",
          info1
        ),
        class = "invalid_lto_file_format"
      )
    }

    # If the "Date" field is missing, stop here
    expected_field_name <- "date"
    if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
      abort(
        str_glue(
          "`file` is missing a required field.\n",
          "{col_red(symbol$cross)} The required field \":: Date\" is missing.\n",
          info1
        ),
        class = "invalid_lto_file_format"
      )
    }

    # If the "Collections" field is missing, stop here
    expected_field_name <- "collection_ids"
    if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
      abort(
        str_glue(
          "`file` is missing a required field.\n",
          "{col_red(symbol$cross)} The required field \":: Collections\" is missing.\n",
          info1
        ),
        class = "invalid_lto_file_format"
      )
    }
    
    # If the collection ID already exists in the LTO active version, stop here
    field_name <- "header"
    reserved_collection_ids <- get_collections_tbl() %>%
      pull(.data$collection_id)
    candidate_collection_id <- parsed_fields_tbl %>%
      filter(.data$name == field_name) %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    if (candidate_collection_id %in% reserved_collection_ids) {
      abort(
        str_glue(
          "`file` contains an invalid collection.\n",
          "{col_red(symbol$cross)} The collection ID \"{collection_id}\" is reserved.\n",
          info1
        ),
        class = "invalid_lto_file_format"
      )
    }

    # If the "Component Stories" field is missing, stop here
    expected_field_name <- "component_story_ids"
    if (!any(str_detect(parsed_fields_tbl$name, pattern = expected_field_name))) {
      abort(
        str_glue(
          "`file` is missing a required field.\n",
          "{col_red(symbol$cross)} The required field \":: Component Stories\" is missing.\n",
          info1
        ),
        class = "invalid_lto_file_format"
      )
    }

    # Parse component stories, storing valid ones
    valid_story_ids <- get_stories_tbl() %>% pull(.data$story_id)
    candidate_component_story_ids <- parsed_fields_tbl %>%
      filter(.data$name == expected_field_name) %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    invalid_story_ids <- setdiff(candidate_component_story_ids, valid_story_ids)
    if (!identical(invalid_story_ids, character(0))) {
      if (verbose) cli_alert_warning("Discarding invalid story IDs:\n", "{str_c(invalid_story_ids, collapse = \"\n\")}")
      component_story_ids <- intersect(candidate_component_story_ids, valid_story_ids)
      parsed_fields_tbl$contents[[which(parsed_fields_tbl$name == expected_field_name)]] <- tibble(contents = component_story_ids)
    }
  }
  
  # Reconfigure `parsed_fields_tbl` into a more familiarly formatted tibble
  field_names <- parsed_fields_tbl$name
  description <- parsed_fields_tbl %>%
      filter(.data$name == "description") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
  if (isTRUE(lto_file_type == "themeset")) {
    themeset_name <- parsed_fields_tbl %>%
      filter(.data$name == "name") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    component_theme_names <- parsed_fields_tbl %>%
      filter(.data$name == "component_theme_names") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    lto_tbl <- tibble(
      themeset_index = NA,
      themeset_id = header,
      themeset_name = themeset_name,
      description = description,
      component_theme_names = list(component_theme_names)
    )
  } else if (isTRUE(lto_file_type == "collection")) {
    title <- parsed_fields_tbl %>%
      filter(.data$name == "title") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    date <- parsed_fields_tbl %>%
      filter(.data$name == "date") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    component_story_ids <- parsed_fields_tbl %>%
      filter(.data$name == "component_story_ids") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    if (isTRUE("references" %in% field_names)) {
      references <- parsed_fields_tbl %>%
      filter(.data$name == "references") %>%
      pull(.data$contents) %>%
      unlist(use.names = FALSE)
    } else {
      references <- tibble(references = character(0))
    }
    lto_tbl <- tibble(
      collection_index = NA,
      collection_id = header,
      title = title,
      description = description,
      date = date,
      component_story_ids = list(component_story_ids),
      references = list(references),
      themes = list(tibble(data.frame(theme_name = character(0), level = character(0), motivation = character(0)))),
      source = NA
    )
  } else {
    lto_tbl <- tibble()
  }

  lto_tbl
}

# Parse an LTO formatted file field entry
parse_field_contents <- function(text, format, field_literal) {
  # If `text` is missing, stop here
  if (is_missing(text)) {
    message <- get_missing_arg_msg(variable_name = "text")
    abort(message, class = "missing_argument")
  }

  # If `format` is missing, stop here
  if (is_missing(format)) {
    message <- get_missing_arg_msg(variable_name = "format")
    abort(message, class = "missing_argument")
  }

  # If `field_literal` is missing, stop here
  if (is_missing(field_literal)) {
    message <- get_missing_arg_msg(variable_name = "field_literal")
    abort(message, class = "missing_argument")
  }

  if (format == "text blocks") {
    return(tibble(contents = remove_wordwrap(text)))
  } else if (format == "multiline") {
    return(tibble(contents = text %>% str_trim() %>% str_split(pattern = "\n") %>% unlist()))
  } else if (format == "single term") {
    text <- str_trim(text)
    return(tibble(contents = text))
  }

  abort(
    str_glue(
      "`format` must be a recognized string or NA.\n",
      "{col_red(symbol$cross)} You supplied the unrecognized value {format}.\n",
      "{col_yellow(symbol$info)} Run `unique(get_field_def_tbl() %>% dplyr::select(format))` to view recognized values."
    )
  )
}

# An LTO file header line must start with one of these hard coded fragments
get_lto_file_type <- function(string) {
  # If `string` is missing, stop here
  if (is_missing(string)) {
    message <- get_missing_arg_msg(variable_name = "string")
    abort(message, class = "missing_argument")
  }

  lto_file_types_tbl <- get_lto_file_info_tbl()

  if (!any(str_starts(string, pattern = lto_file_types_tbl$header_fragment))) {
    abort(
      str_glue(
        "`string` corresponds to an invalid file type.\n",
        "{symbol$cross} The file's first line must start with one of the prefixes\n",
        "{str_c(lto_file_types_tbl$header_fragment, collapse = \"\n)}\""
      ),
      class = "invalid_file_type"
    )
  }
  
  lto_file_types_tbl$type[str_which(string, pattern = lto_file_types_tbl$header_fragment)]
}

# Hard coded constraints for parsing LTO files
get_field_def_tbl <- function() {
  field_names <- c(
    "choice_themes",
    "collection_ids",
    "component_story_ids",
    "component_theme_names",
    "date",
    "description",
    "major_themes",
    "minor_themes",
    "notes",
    "references",
    "title"
  )

  field_literals <- c(
    ":: Choice Themes",
    ":: Collections",
    ":: Component Stories",
    ":: Component Themes",
    ":: Date",
    ":: Description",
    ":: Major Themes",
    ":: Minor Themes",
    ":: Notes",
    ":: References",
    ":: Title"
  )

  field_formats <- c(
    NA,
    "multiline",
    "multiline",
    "multiline",
    "single term",
    "text blocks",
    NA,
    NA,
    "text blocks",
    "multiline",
    "single term"
  )

  field_statuses <- c(
    "unsupported",
    "supported",
    "supported",
    "supported",
    "supported",
    "supported",
    "unsupported",
    "unsupported",
    "supported",
    "supported",
    "supported"
  )

  field_def_tbl <- tibble(
    name = field_names,
    literal = field_literals,
    format = field_formats,
    status = field_statuses
  )
 
  field_def_tbl
}
