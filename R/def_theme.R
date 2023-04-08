#' R6 class representing an LTO literary theme
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' The \pkg{stoRy} package uses the `Theme` R6 class to represent individual
#' LTO literary themes. This class is mostly useful for accessing information
#' about an LTO theme for which the theme name is known in advance.
#'
#' @details
#' The class operates on the themes of whichever LTO version happens to be
#' actively loaded into the \pkg{stoRy} package level environment. This is
#' the LTO `demo` version by default. Run [which_lto()] to check which LTO
#' version is active in your R session.
#'
#' Search the latest LTO `dev` version themes on the Theme Ontology website at
#' \url{https://www.themeontology.org/themes}.
#' 
#' @param ... Additional arguments.
#'
#' @seealso Use [Collection()] to initialize an collection of LTO thematically
#'   annotated stories.
#' @seealso Use [Story()] to initialize an LTO thematically annotated story.
#' @seealso Use [Themeset()] to initialize a set of related LTO themes.
#'
#' @docType class
#' 
#' @export Theme
#' @examples \dontrun{
#' # Initialize an LTO `demo` version theme pertaining to Martians:
#' set_lto("demo")
#' theme <- Theme$new(theme_name = "Martian extraterrestrial")
#' 
#' # Print theme info to console:
#' theme
#' 
#' # Print theme info to console in the canonical th.txt format:
#' theme$print(canonical = TRUE)
#'
#' # Return the theme description:
#' theme$description()
#'
#' # Return references associated with the description, if any:
#' theme$references()
#' 
#' # Return theme name aliases, if any:
#' theme$aliases()
#'
#' # Return the theme's parent theme names:
#' theme$parents()
#'
#' # Return the theme's ancestor theme names:
#' theme$ancestors()
#'
#' # Return a tibble of thematically annotated stories:
#' theme$annotations()
#' }
Theme <- R6::R6Class(
  classname = "Theme",
  public = list(
  	#' @description
    #' Initialize an LTO theme.
    #' @param theme_name A length-one character vector corresponding to an LTO
    #'   theme name.
    #' @return A new `Theme` object.
    initialize = function(theme_name) {
      # If `theme_name` is missing, stop here
      if (is_missing(theme_name)) {
        message <- get_missing_arg_msg(variable_name = "theme_name")
        abort(message, class = "missing_argument")
      }

      # If `theme_name` is not a single string, stop here
      if (isTRUE(!is.character(theme_name) || length(theme_name) != 1)) {
        message <- get_single_string_msg(string = theme_name, variable_name = "theme_name")
        abort(message, class = "function_argument_type_check_fail")
      }
      
      # If `theme_name` does not correspond to an LTO theme, stop here
      theme <- get_themes_tbl() %>% filter(.data$theme_name == !!theme_name)
      pulled_theme_name <- theme %>% pull(var = theme_name)
      if (identical(pulled_theme_name, character(0))) {
        abort(str_glue(
          "`theme_name` does not exist.\n",
          "{col_red(symbol$cross)} You've supplied a theme that does not exist in LTO version {stoRy_env$active_version}.")
        )
      }

      # Store the `theme` tibble as private to stop the user from modifying it
      private$theme <- theme
    },
    #' @description
    #' return A length-one character vector corresponding to the theme name.
    theme_name = function() {
      private$theme %>% pull(.data$theme_name)
    },
    #' @description
    #' return A tibble of theme name aliases.
    aliases = function() {
      private$theme %>% select(.data$aliases) %>% unnest(cols = .data$aliases)
    },
    #' @description
    #' return A length-one character vector corresponding to the theme
    #'   description.
    description = function() {
      private$theme %>% pull(.data$description)
    },
    #' @description
    #' return A tibble of caveats that accompany that theme description. This
    #'   is empty for the vast majority of themes. 
    notes = function() {
      private$theme %>% select(.data$notes) %>% unnest(cols = .data$notes)
    },
    #' @description
    #' return A tibble of references.
    references = function() {
      private$theme %>% 
        select(.data$references) %>%
        unnest(cols = .data$references)
    },
    #' @description
    #' return A tibble of example usages of the theme, if any. 
    examples = function() {
      private$theme %>% 
        select(.data$examples) %>%
        unnest(cols = .data$examples)
    },
    #' @description
    #' return A tibble of the theme's parent theme names.
    parents = function() {
      private$theme %>% select(.data$parents) %>% unnest(cols = .data$parents)
    },
    #' @description
    #' return A tibble of the theme's ancestor theme names.
    ancestors = function() {
      private$theme %>% 
        select(.data$ancestors) %>%
        unnest(cols = .data$ancestors)
    },
    #' @description
    #' return The path of the th.txt file containing the theme. This is the
    #'   file path as it occurs on the Theme Ontology GitHub repository at
    #'   \url{https://github.com/theme-ontology/theming}.
    source = function() {
      private$theme %>% pull(.data$source)
    },
    #' @description
    #' return A tibble with one row for each story in which the theme is
    #'   featured. The first column is the LTO story ID, the second is the
    #'   release data, the third is the level (choice/major/minor) at which
    #'   the theme is featured, and the fourth a justification for applying
    #'   the theme. Each column is of type character. 
    annotations = function() {
      get_thematic_annotations_tbl(self$theme_name())
    },
    #' @description
    #' Print theme object info to console.
    #' @template canonical-arg
    #' @template width-arg
    print = function(canonical = FALSE,
                     width = NULL,
                     ...) {
      # Set a custom maximum column width for printed output, if desired
      if (is.null(width)) width <- stoRy_opt("width")
      
      # The header format does not depend on the `canonical` value
      theme_name <- self$theme_name()
      thematic_annotations <- get_thematic_annotations_tbl(theme_name)
      number_of_thematic_annotations <- nrow(thematic_annotations)
      lto_version_tag <- which_lto()
      if (isTRUE(number_of_thematic_annotations == 1 && lto_version_tag %in% c("demo", "dev"))) {
        header <- str_glue("A theme featured in {number_of_thematic_annotations} LTO {lto_version_tag} version story")
      } else if (isTRUE(number_of_thematic_annotations == 1 && !(lto_version_tag %in% c("demo", "dev")))) {
        header <- str_glue("A theme featured in {number_of_thematic_annotations} LTO {lto_version_tag} story")
      } else if (isTRUE(number_of_thematic_annotations != 1 && lto_version_tag %in% c("demo", "dev"))) {
        header <- str_glue("A theme featured in {number_of_thematic_annotations} LTO {lto_version_tag} version stories")
      } else {
        header <- str_glue("A theme featured in {number_of_thematic_annotations} LTO {lto_version_tag} stories")
      }
      comment <- format_comment(header, width = min(width, stoRy_opt("width")))
      cat(comment, sep = "\n")

      # Retrieve theme object contents to facilitate printing to console
      description <- self$description()
      notes <- self$notes() %>% unlist(use.names = FALSE)
      references <- self$references() %>% unlist(use.names = FALSE)
      parents <- self$parents() %>% unlist(use.names = FALSE)
      aliases <- self$aliases() %>% unlist(use.names = FALSE)
      examples <- self$examples() %>% unlist(use.names = FALSE)

      # Print theme info to console
      if (canonical) {
        # Print theme name
        cat(str_glue("{theme_name}\n"), sep = "\n")
        cat(str_glue("{get_underline(string = theme_name)}\n"), sep = "\n")
        
        # Print theme definition
        cat("\n:: Description", sep = "\n")
        cat(str_wrap(description, width = width), sep = "\n")

        # Print definition caveats, if any 
        if (!identical(notes, character(0))) {
          cat("\n:: Notes", sep = "\n")
          for (i in seq_along(notes)) {
            cat(str_wrap(notes[i], width = width, indent = 0), sep = "\n")
            if (isTRUE(i < length(notes))) cat("\n")
          }
        }

        # Print reference urls, if any
        if (!identical(references, character(0))) {
          cat("\n:: References", sep = "\n")
          cat(str_c(references), sep = "\n")
        }

        # Print parent theme names, if any
        if (!identical(parents, character(0))) {
          cat("\n:: Parents", sep = "\n")
          cat(str_c(parents), sep = "\n")
        }

        # Print theme name aliases, if any
        if (!identical(aliases, character(0))) {
          cat("\n:: Aliases", sep = "\n")
          cat(str_c(aliases), sep = "\n")
        }

        # Print example theme usages, if any
        if (!identical(examples, character(0))) {
          cat("\n:: Examples", sep = "\n")
          for (i in seq_along(examples)) {
            cat(
              str_wrap(
                examples[[i]],
                width = width,
                indent = 0
              ),
              sep = "\n"
            )
            if (isTRUE(i < length(examples))) cat("\n")
          }
        }
      } else {
        # Print theme name
        cat(
          str_wrap2(
            string1 = "Theme Name",
            string2 = theme_name,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )
        
        # Print theme definition
        cat(
          str_wrap2(
            string1 = "Description",
            string2 = description,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )
        
        # Print definition caveats, if any
        if (!identical(notes, character(0))) {
          for (i in seq_along(notes)) {
            if (isTRUE(i == 1)) {
              cat(
                str_wrap2(
                  string1 = "Notes",
                  string2 = notes[[i]],
                  width = width,
                  exdent = 2
                ),
                sep = "\n"
              )
            } else {
              cat(
                str_wrap(
                  notes[[i]],
                  width = width,
                  indent = 2,
                  exdent = 2
                ),
                sep = "\n"
              )
            }
            if (isTRUE(i < length(notes))) cat("\n")
          }
        }

        # Print definition references, if any
        if (!identical(references, character(0))) {
          cat(str_glue(black$bold("References"), ":\n"), sep = "\n")
          cat(str_wrap(references, indent = 2), sep = "\n")
        }

        # Print parent theme names, if any
        if (!identical(parents, character(0))) {
          cat(
            str_wrap(
              str_glue(
                black$bold("Parents"), ": {str_c(parents, collapse = ", ")}"
              ),
              width = width,
              exdent = 2
            ),
            sep = "\n"
          )
        }

        # Print theme name aliases, if any
        if (!identical(aliases, character(0))) {
          cat(
            str_wrap(
              str_glue(
                black$bold("Aliases"), ": {str_c(aliases, collapse = ", ")}"
              ),
              width = width,
              exdent = 2
            ),
            sep = "\n"
          )
        }

        # Print example theme usages, if any
        if (length(examples) > 0) {
          for (i in seq_along(examples)) {
            if (isTRUE(i == 1)) {
              cat(
                str_wrap2(
                  string1 = "Examples",
                  string2 = examples[[i]],
                  width = width,
                  exdent = 2
                ),
                sep = "\n"
              )
            } else {
              cat(
                str_wrap(
                  examples[[i]],
                  width = width,
                  indent = 2,
                  exdent = 2
                ),
                sep = "\n"
              )
            }
            if (isTRUE(i < length(examples))) cat("\n")
          }
        }
      }
    }
  ),
  private = list(
    theme = NULL
  )
)
