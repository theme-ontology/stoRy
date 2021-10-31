#' R6 class representing an LTO thematically annotated story
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' The \pkg{stoRy} package uses the `Story` R6 class to represent the LTO
#' thematic annotations for individual works of fiction. This class is mostly
#' useful for accessing information about an LTO thematically annotated story
#' for which the story ID is known in advance.
#'
#' @details
#' The class operates on the stories of whichever LTO version happens to be
#' actively loaded into the \pkg{stoRy} package level environment. This is
#' the LTO `demo` version by default. Run [which_lto()] to check which LTO
#' version is active in your R session.
#'
#' Search the latest LTO `dev` version stories on the Theme Ontology website
#' at \url{https://themeontology.org/stories}.
#'
#' @param ... Additional arguments
#'
#' @seealso Use [Collection()] to initialize an collection of LTO thematically
#'   annotated stories.
#' @seealso Use [Theme()] to initialize an LTO theme.
#' @seealso Use [Themeset()] to initialize a set of related LTO themes.
#'
#' @docType class
#'
#' @export Story
#' @examples \dontrun{
#' # Initialize the LTO `demo` version of a classic The Twilight Zone (1959) story:
#' set_lto("demo")
#' story <- Story$new(story_id = "tz1959e1x22")
#' 
#' # Print story info and thematic annotations to console:
#' story
#' 
#' # Print story info and thematic annotations in st.txt format:
#' story$print(canonical = TRUE)
#'
#' # Return the story title:
#' story$title()
#'
#' # Return the story description:
#' story$description()
#'
#' # Return a tibble of thematic annotations:
#' story$themes()
#' }
Story <- R6::R6Class(
  classname = "Story",
  public = list(
  	#' @description
    #' Initialize an LTO thematically annotated story.
    #' @param story_id A length-one character vector corresponding to the ID
    #'   of an LTO thematically annotated work of fiction.
    #' @return A new `Story` object.
    initialize = function(story_id) {
      # If `story_id` is missing, stop here
      if (is_missing(story_id)) {
        message <- get_missing_arg_msg(variable_name = "story_id")
        abort(message, class = "missing_argument")
      }

      # If `story_id` is not a single string, stop here
      if (isTRUE(!is.character(story_id) || length(story_id) != 1)) {
        message <- get_single_string_msg(string = story_id, variable_name = "story_id")
        abort(message, class = "function_argument_type_check_fail")
      }
      
      # If `story_id` does not correspond to an LTO story, stop here
      story <- get_stories_tbl() %>% filter(.data$story_id == !!story_id)
      pulled_story_id <- story %>% pull(var = story_id)
      if (identical(pulled_story_id, character(0))) {
        message <- get_invalid_lto_id_msg(id_class = "story_id")
        abort(message, class = "invalid_lto_id")
      }

      # Store the `story` tibble as private to stop the user from modifying it
      private$story <- story

      # Precompute this tibble to use internally in package functions 
      private$internal_tbl <- story %>%
        select(.data$themes) %>%
        unnest(cols = .data$themes) %>%
        add_column(story_id = story %>%
        pull(.data$story_id), .before = 1) %>%
        select(-.data$motivation) %>%
        mutate(
          ancestor_themes = vget_ancestor_theme_names(
          theme_name = .data$theme_name, 
          blacklist_theme_names = "literary thematic entity",
          return_self = TRUE), .after = "story_id") %>% 
        unnest(cols = .data$ancestor_themes) %>%
        mutate(explicit = ifelse(isTRUE(theme_name == ancestor_themes), TRUE, FALSE)) %>%
        select(-.data$theme_name) %>%
        rename(theme_name = .data$ancestor_themes)
    },
    #' @description
    #' return A length-one character vector corresponding to the story ID.
    story_id = function() {
      private$story %>% pull(.data$story_id)
    },
    #' @description
    #' return A length-one character vector corresponding to the story title.
    title = function() {
      private$story %>% pull(.data$title)
    },
    #' @description
    #' return A length-one character vector corresponding to some summary
    #'   information about the story. This is typically a synopsis and/or
    #'   details about the authorship, production, distribution, etc.
    description = function() {
      private$story %>% pull(.data$description)
    },
    #' @description
    #' return A length-one character vector corresponding to the story release
    #'   date.
    date = function() {
      private$story %>% pull(.data$date)
    },
    #' @description
    #' return A tibble of story reference urls, if any.
    references = function() {
      private$story %>%
        select(.data$references) %>%
        unnest(cols = .data$references)
    },
    #' @description
    #' return A tibble of LTO collections to which the story belongs, if any.
    collections = function() {
      private$story %>%
        select(.data$collections) %>%
        unnest(cols = .data$collections)
    },
    #' @description
    #' return A tibble of thematic annotations.
    themes = function() {
      private$story %>% select(.data$themes) %>% unnest(cols = .data$themes)
    },
    #' @description
    #' return The path of the st.txt file containing the story thematic
    #'   annotations. This is the file path as it occurs on the Theme Ontology
    #'   GitHub repository at \url{https://github.com/theme-ontology/theming}.
    source = function() {
      private$story %>% pull(.data$source)
    },
    #' @description
    #'   return A special tibble that is used internally by package functions.
    obj_internal_tbl = function() {
      private$internal_tbl
    },
    #' @description
    #' Print story object info to console.
    #' @template canonical-arg
    #' @template width-arg
    print = function(canonical = FALSE,
                     width = NULL,
                     ...) {
      # Set a custom maximum column width for printed output, if desired
      if (is.null(width)) width <- stoRy_opt("width")
      
      # Retrieve story object contents to facilitate printing to console
      story_id <- self$story_id()
      title <- self$title()
      date <- self$date()
      collections <- self$collections() %>% unlist(use.names = FALSE)
      description <- self$description()
      references <- self$references() %>% unlist(use.names = FALSE)
      themes <- self$themes()
      choice_themes <- themes %>% filter(level == "choice")
      major_themes <- themes %>% filter(level == "major")
      minor_themes <- themes %>% filter(level == "minor")

      # The header format does not depend on the `canonical` value
      number_of_thematic_annotations <- nrow(themes)
      lto_version_tag <- which_lto()
      if (isTRUE(number_of_thematic_annotations == 1 && lto_version_tag %in% c("demo", "dev"))) {
        header <- str_glue("A story annotated with {number_of_thematic_annotations} LTO {lto_version_tag} version themes")
      } else if (isTRUE(number_of_thematic_annotations == 1 && !(lto_version_tag %in% c("demo", "dev")))) {
        header <- str_glue("A story annotated with {number_of_thematic_annotations} LTO {lto_version_tag} themes")
      } else if (isTRUE(number_of_thematic_annotations != 1 && lto_version_tag %in% c("demo", "dev"))) {
        header <- str_glue("A story annotated with {number_of_thematic_annotations} LTO {lto_version_tag} version themes")
      } else {
        header <- str_glue("A story annotated with {number_of_thematic_annotations} LTO {lto_version_tag} themes")
      }
      comment <- format_comment(header, width = min(width, stoRy_opt("width")))
      cat(comment, sep = "\n")

      # Print story info to console
      if (canonical) {
        # Print story ID
        cat(str_glue("{story_id}\n"), sep = "\n")
        cat(str_glue("{get_underline(string = story_id)}\n"), sep = "\n")

        # Print story title
        cat("\n:: Title", sep = "\n")
        cat(str_glue("{title}\n"), sep = "\n")

        # Print story release date
        cat("\n:: Date", sep = "\n")
        cat(str_glue("{date}\n"), sep = "\n")

        # Print collection of which the story is a member, if any
        if (!identical(collections, character(0))) {
          cat("\n:: Collections", sep = "\n")
          cat(str_c(collections), sep = "\n")
        }

        # Print story summary
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
      } else {
        # Print story ID
        cat(
          str_wrap2(
            string1 = "Story ID",
            string2 = story_id,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

        # Print story title
        cat(
          str_wrap2(
            string1 = "Title",
            string2 = title,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

        # Print story release date
        cat(str_glue(black$bold("Date"), ": {date}\n"), sep = "\n")

        # Print collection of which the story is a member, if any
        if (!identical(collections, character(0))) {
          cat(str_glue(black$bold("Collections"), ":\n"), sep = "\n")
          cat(str_wrap(collections, indent = 2), sep = "\n")
        }
        
        # Print story summary
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
      }
    }
  ),
  private = list(
    story = NULL,
    internal_tbl = NULL
  )
)




