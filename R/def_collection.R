#' R6 class representing a collection of LTO thematically annotated stories
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' The \pkg{stoRy} package uses the `Collection` R6 class to represent a set
#' of related LTO thematically annotated stories. This class is mostly useful
#' for accessing information about a collection of stories for which the
#' collection ID is known in advance.
#' 
#' @details
#' The class operates on the story collection of whichever LTO version happens
#' to be actively loaded into the \pkg{stoRy} package level environment. This
#' is the LTO `demo` version by default. Run [which_lto()] to check which LTO
#' version is active in your R session.
#'
#' Search the latest LTO `dev` version collections on the Theme Ontology
#' website at \url{https://www.themeontology.org/stories}.
#'
#' Alternatively, it is possible to read in a user-defined collection from
#' file. In this case, the collection ID as defined in the file must match the
#' `collection_id` input parameter.
#'
#' @param ... Additional arguments
#'
#' @seealso Use [Story()] to initialize an LTO thematically annotated story.
#' @seealso Use [Theme()] to initialize an LTO theme.
#' @seealso Use [Themeset()] to initialize a set of related LTO themes.
#'
#' @docType class
#'
#' @export Collection
#' @examples \dontrun{
#' # Initialize a collection:
#' set_lto("demo")
#' collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
#'
#' # Print collection info to console:
#' collection
#'
#' # Print collection info in canonical st.txt format:
#' collection$print(canonical = TRUE)
#'
#' # Initialize a collection from file:
#' set_lto("demo")
#' file <- system.file("extdata/rolling-stone-best-ttz1959-episodes.st.txt", package = "stoRy")
#' collection_id <- "Collection: Rolling Stone 25 Best Twilight Zone Original Series Episodes"
#' collection <- Collection$new(collection_id, file)
#' collection
#'
#' # Initialize a collection from a string:
#' set_lto("demo")
#' file <- I("Collection: Rolling Stone 25 Best Twilight Zone Original Series Episodes
#'========================================================================
#'
#':: Title
#'Rolling Stone 25 Best Twilight Zone Original Series Episodes
#'
#':: Date
#'1959-1964
#'
#':: Description
#'Rolling Stone Magazine's list of the 25 best episodes from the original
#'Twilight Zone anthology television series, which ran for five seasons on CBS
#'from 1959 to 1964, as compiled by David Fear, Sean T. Collins, and Angie
#'Martoccio.
#'
#':: References
#'https://www.rollingstone.com/tv/tv-features/25-best-twilight-zone-episodes-list-812043/
#'
#':: Collections
#'Collection: Rolling Stone 25 Best Twilight Zone Original Series Episodes
#'
#':: Component Stories
#'tz1959e3x24
#'tz1959e1x22
#'tz1959e2x06
#'tz1959e5x03
#'tz1959e2x15
#'tz1959e2x28
#'tz1959e1x08
#'tz1959e3x14
#'tz1959e3x05
#'tz1959e5x06
#'tz1959e3x08
#'tz1959e1x01
#'tz1959e1x21
#'tz1959e1x34
#'tz1959e2x07
#'tz1959e1x13
#'tz1959e1x09
#'tz1959e3x10
#'tz1959e1x16
#'tz1959e1x28
#'tz1959e1x30
#'tz1959e3x33
#'tz1959e3x01
#'tz1959e2x22
#'tz1959e5x25")
#' collection_id <- unlist(strsplit(file, split = "\n"))[1]
#' collection <- Collection$new(collection_id, file)
#' collection
#' }
Collection <- R6::R6Class(
  classname = "Collection",
  parent_env = asNamespace('stoRy'),
  public = list(
  	#' @description
    #' Initialize a collection of LTO thematically annotated stories.
    #' @param collection_id A length-one character vector corresponding to the
    #'   ID of an LTO collection of stories.
    #' @param file A file name of a collection file or path to a collection
    #'   file or a string. Files must end with the standard .st.txt extension
    #'   used for story and collection files.
    #'   
    #'   If `file` is a file name, then the file is assumed to reside in the
    #'   current working directory.
    #' @template verbose-arg
    #' @return A new `Collection` object.
    initialize = function(
      collection_id,
      file = NULL,
      verbose = TRUE) {
      # If `collection_id` is missing, stop here
      if (is_missing(collection_id)) {
        message <- get_missing_arg_msg(variable_name = "collection_id")
        abort(message, class = "missing_argument")
      }

      # If `collection_id` is not a single string, stop here
      if (isTRUE(length(collection_id) != 1 || !is.character(collection_id))) {
        message <- get_single_string_msg(string = collection_id, variable_name = "collection_id")
        abort(message, class = "function_argument_type_check_fail")
      }
      
      # If `file` is NULL, try to initialize as an existing collection;
      # otherwise try to initialize a user collection from file
      if (is.null(file)) {
        collection <- get_collections_tbl() %>%
          filter(.data$collection_id == !!collection_id)
        pulled_collection_id <- collection %>% pull(var = collection_id)

        # If `collection_id` does not correspond to an LTO collection, stop here
        if (identical(pulled_collection_id, character(0))) {
          message <- get_invalid_lto_id_msg(id_class = "collection_id")
          abort(message, class = "invalid_lto_id")
        }
      } else {
        # Return the collection in the form of a character vector of strings;
        # one string for each line of the file
        lines <- lto_file_to_lines(file, type = "collection")

        # Parse `lines` and store results in tibble format
        collection <- lto_file_to_tbl(lines, verbose)
      }

      # Store the `collection` tibble as private to prevent the user from
      # modifying its contents
      private$collection <- collection

      # Precompute this tibble to use internally in package functions 
      private$internal_tbl <- private$collection %>%
        select(.data$component_story_ids) %>% 
        unnest(cols = .data$component_story_ids) %>%
        rename(story_id = .data$component_story_ids) %>%
        inner_join(get_stories_tbl(), by = "story_id") %>%
        select(.data$story_id, .data$themes) %>%
        unnest(col = .data$themes) %>%
        select(-.data$motivation) %>%
        mutate(
          ancestor_themes = vget_ancestor_theme_names(
          theme_name = .data$theme_name, 
          blacklist_theme_names = "literary thematic entity",
          return_self = TRUE), .after = "story_id") %>% 
        unnest(cols = .data$ancestor_themes) %>%
        mutate(explicit = ifelse(theme_name == ancestor_themes, TRUE, FALSE)) %>%
        select(-.data$theme_name) %>%
        rename(theme_name = .data$ancestor_themes)
    },
    #' @description
    #' return A length-one character vector corresponding to the collection
    #'   ID.
    collection_id = function() {
      private$collection %>% pull(.data$collection_id)
    },
    #' @description
    #' return A length-one character vector corresponding to the collection
    #'   title.
    title = function() {
      private$collection %>% pull(.data$title)
    },
    #' @description
    #' return A length-one character vector of collection defining text.
    description = function() {
      private$collection %>% pull(.data$description)
    },
    #' @description
    #' return A length-one character vector typically of the form "yyyy-yyyy"
    #'   indicating the start and end year for stories in the collection.
    date = function() {
      private$collection %>% pull(.data$date)
    },
    #' @description
    #' return A tibble of collection reference urls, if any.
    references = function() {
      private$collection %>% 
        select(.data$references) %>%
        unnest(cols = .data$references)
    },
    #' @description
    #' return A tibble of member story IDs.
    component_story_ids = function() {
      private$collection %>%
        select(.data$component_story_ids) %>%
        unnest(cols = .data$component_story_ids)
    },
    #' @description
    #' return A tibble of thematic annotations.
    themes = function() {
      private$collection %>%
        select(.data$themes) %>%
        unnest(cols = .data$themes)
    },
    #' @description
    #' return The path of the st.txt collection file. This is the file path as
    #'   it occurs on the Theme Ontology GitHub repository at 
    #'   \url{https://github.com/theme-ontology/theming}.
    source = function() {
      private$story %>% pull(.data$source)
    },
    #' @description
    #' return A length-one numeric vector containing the number of stories in
    #'   the collection.
    size = function() {
      nrow(self$component_story_ids())
    },
    #' @description
    #' return A special tibble that is used internally by package functions.
    obj_internal_tbl = function() {
      private$internal_tbl
    },
    #' @description
    #' Print collection object info to console.
    #' @template canonical-arg
    #' @param n Maximum number of component story IDs to print to console.
    #' This defaults to NULL which means the
    #' \code{stoRy_opt("print_min")} value is used. Run
    #' \code{options(stoRy.print_min = 25L)} to set the minimum number of
    #' printed component story IDs to be 25. Run
    #' \code{stoRy_opt("print_max")} to check the maximum number of stories
    #' that can be printed to console. This value can be changed in the same
    #' way as with \code{stoRy.print_min}.
    #' @template width-arg
    print = function(canonical = FALSE,
                     n = NULL,
                     width = NULL,
                     ...) {
      # Set a custom maximum column width for printed output, if desired
      if (is.null(width)) width <- stoRy_opt("width")

      # Retrieve collection object contents to facilitate printing to console
      collection_id <- self$collection_id()
      title <- self$title()
      date <- self$date()
      description <- self$description()
      references <- self$references() %>% unlist(use.names = FALSE)
      themes <- self$themes()
      choice_themes <- themes %>% filter(level == "choice")
      major_themes <- themes %>% filter(level == "major")
      minor_themes <- themes %>% filter(level == "minor")
      component_story_ids <- self$component_story_ids() %>%
        pull(.data$component_story_ids)
      size <- self$size()
      max_story_ids <- get_number_of_printed_entries(n, number_of_entries = size)
      number_of_unprinted_entries <- size - max_story_ids

      # The header format does not depend on the `canonical` value
      lto_version_tag <- which_lto()
      header <- str_glue("A collection consisting of {size} component stories")
      comment <- format_comment(header, width = min(width, stoRy_opt("width")))
      cat(comment, sep = "\n")
      
      # Print collection info to console
      if (canonical) {
        # Print collection ID
        cat(str_glue("{collection_id}\n"), sep = "\n")
        cat(str_glue("{get_underline(string=collection_id)}\n"), sep = "\n")

        # Print title
        cat("\n:: Title", sep = "\n")
        cat(str_glue("{title}\n"), sep = "\n")

        # Print collection dates
        cat("\n:: Date", sep = "\n")
        cat(str_glue("{date}\n"), sep = "\n")

        # This redundant printing of the collection ID may be removed in the
        # future
        cat("\n:: Collections", sep = "\n")
        cat(collection_id, sep = "\n")
        
        # Print collection definition
        cat("\n:: Description", sep = "\n")
        cat(str_wrap(description, width = width), sep = "\n")
        
        # Print reference urls, if any
        if (!identical(references, character(0))) {
          cat("\n:: References", sep = "\n")
          cat(str_c(references), sep = "\n")
        }
        
        # Print themes together with motivations
        if (isTRUE(nrow(choice_themes) > 0)) {
          cat("\n:: Choice Themes", sep = "\n")
          for (i in seq.int(nrow(choice_themes))) {
            cat(
              str_c(
                  choice_themes$theme_name[i], " [",
                  choice_themes$motivation[i], "]"
                ),
              sep = "\n"
            )
          }
        }
        if (isTRUE(nrow(major_themes) > 0)) {
          cat("\n:: Major Themes", sep = "\n")
          for (i in seq.int(nrow(major_themes))) {
            cat(
              str_c(
                major_themes$theme_name[i], " [",
                major_themes$motivation[i], "]" 
              ),
              sep = "\n"
            )
          }
        }
        if (isTRUE(nrow(minor_themes) > 0)) {
          cat("\n:: Minor Themes", sep = "\n")
          for (i in seq.int(nrow(minor_themes))) {
            cat(
              str_c(
                minor_themes$theme_name[i], " [",
                minor_themes$motivation[i], "]"
              ),
              sep = "\n"
            )
          }
        }
        
        # Print up to `n` member story IDs
        if (isTRUE(size > 0)) {
          cat("\n:: Component Stories", sep = "\n")
          cat(str_c(component_story_ids[1 : max_story_ids]), sep = "\n")
        }
      } else {
        # Print collection ID
        cat(
          str_wrap2(
            string1 = "Collection ID",
            string2 = collection_id,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

        # Print title
        cat(
          str_wrap2(
            string1 = "Title",
            string2 = title,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

        # Print collection dates
        cat(str_glue(black$bold("Date"), ": {date}\n"), sep = "\n")

        # Print collection definition
        cat(
          str_wrap2(
            string1 = "Description",
            string2 = description,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

        # Print reference urls, if any
        if (!identical(references, character(0))) {
          cat(str_glue(black$bold("References"), ":\n"), sep = "\n")
          cat(str_wrap(references, indent = 2), sep = "\n")
        }

        # Print themes together with motivations
        if (isTRUE(nrow(choice_themes) > 0)) {
          cat(str_glue(black$bold("Choice Themes"), ":\n"), sep = "\n")
          for (i in seq.int(nrow(choice_themes))) {
            cat(
              str_wrap2(
                string1 = choice_themes$theme_name[i],
                string2 = choice_themes$motivation[i],
                width = width,
                indent = 2,
                exdent = 4
              ),
              sep = "\n"
            )
          }
        }
        if (isTRUE(nrow(major_themes) > 0)) {
          cat(str_glue(black$bold("Major Themes"), ":\n"), sep = "\n")
          for (i in seq.int(nrow(major_themes))) {
            cat(
              str_wrap2(
                string1 = major_themes$theme_name[i],
                string2 = major_themes$motivation[i],
                width = width,
                indent = 2,
                exdent = 4
              ),
              sep = "\n"
            )
          }
        }
        if (isTRUE(nrow(minor_themes) > 0)) {
          cat(str_glue(black$bold("Minor Themes"), ":\n"), sep = "\n")
          for (i in seq.int(nrow(minor_themes))) {
            cat(
              str_wrap2(
                string1 = minor_themes$theme_name[i],
                string2 = minor_themes$motivation[i],
                width = width,
                indent = 2,
                exdent = 4
              ),
              sep = "\n"
            )
          }
        }

        # Print up to `n` member story IDs
        if (isTRUE(size > 0)) {
          cat(str_glue(black$bold("Component Story IDs"), ":\n"), sep = "\n")
          
          for (i in seq.int(max_story_ids)) {
            cat(
              str_wrap(
                component_story_ids[i],
                width = width,
                indent = 2
              ),
              sep = "\n"
            )
          }
        }
      }

      # Print footer, if need be
      if (isTRUE(number_of_unprinted_entries > 0)) {
        footer <- pre_dots(str_glue("with {number_of_unprinted_entries} more component stories"))
        comment <- format_comment(footer, width = min(width, stoRy_opt("width")))
        cat(comment, sep = "\n")
      }
    }
  ),
  private = list(
    collection = NULL,
    internal_tbl = NULL
  )
)




