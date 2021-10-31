#' R6 class representing an LTO themeset
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The \pkg{stoRy} package uses the `Themeset` R6 class to represent user
#' defined collections of themes. Themesets are currently in an experimental
#' stage of development, but can be expected to become an integrate part of
#' future \pkg{stoRy} package analysis functions.
#'
#' @details Various themesets are hosted on the themesets Theme Ontology
#'   GitHub repository <https://github.com/theme-ontology/themesets>.
#' 
#' @param ... Additional arguments.
#'
#' @seealso Use [Collection()] to initialize an collection of LTO thematically
#'   annotated stories.
#' @seealso Use [Story()] to initialize an LTO thematically annotated story.
#' @seealso Use [Theme()] to initialize an LTO theme.
#'
#' @docType class
#'
#' @export Themeset
#' @examples \dontrun{
#' # Initialize a themeset from file:
#' set_lto("demo")
#' file <- system.file("extdata/immortality.thset.txt", package = "stoRy")
#' themeset <- Themeset$new(file)
#'
#' # Print themeset info to console:
#' themeset
#'
#' #' # Read themeset from a url and print to console:
#' set_lto("demo")
#' file <- paste0(
#'   "https://raw.githubusercontent.com/theme-ontology/",
#'   "master/demo/immortality.thset.txt"
#' )
#' themeset <- Themeset$new(file)
#' themeset
#'
#' # Initialize a themeset directly from a string and print to console:
#' set_lto("demo")
#' file <- I("Themeset: immortality
#'=====================
#'
#':: Description
#'Themes related to people living on well beyond what is considered to be a
#'normal human lifespan.
#'
#':: Component Themes
#'immortality
#'the flip side of immortality
#'the quest for immortality")
#' themeset <- Themeset$new(file)
#' themeset 
#' }
Themeset <- R6::R6Class(
  classname = "Themeset",
  parent_env = asNamespace('stoRy'),
  public = list(
  	#' @description
    #' Initialize a collection of LTO themes.
    #'
    #' @param file Either a file name, a path to a file, a url, or a single 
    #'   single must contain at least one newline to be recognized as such (as
    #'   string as opposed to a path or url). Files must end with the standard
    #'   .thset.txt extension used for themeset files.
    #'   
    #'   If `file` is a file name, then the file is assumed to reside in the
    #'   current working directory.
    #' @template verbose-arg
    #' @return A new `Themeset` object.
    initialize = function(file, verbose = TRUE) {
      # If `file` is missing, stop here
      if (is_missing(file)) {
        message <- get_missing_arg_msg(variable_name = "file")
        abort(message, class = "missing_argument")
      }

      # Return the themeset in the form of a character vector of strings; one
      # string for each line of the file
      lines <- lto_file_to_lines(file, type = "themeset")
      
      # Read and parse `file` contents
      themeset <- lto_file_to_tbl(lines)

      # Store the `themeset` tibble as private to prevent the user from
      # modifying its contents
      private$themeset <- themeset
    },
    #' @description
    #' return A length-one character vector corresponding to the themeset ID. 
    themeset_id = function() {
      private$themeset %>% pull(.data$themeset_id)
    },
    #' @description
    #' return A length-one character vector corresponding to the themeset
    #'   description. 
    description = function() {
      private$themeset %>% pull(.data$description)
    },
    #' @description
    #' return A tibble of member themes.
    component_theme_names = function() {
      private$themeset %>%
        select(.data$component_theme_names) %>%
        unnest(cols = .data$component_theme_names)
    },
    #' @description
    #' return A length-one numeric vector containing the number of themes in
    #'   the themeset.
    size = function() {
      nrow(self$component_theme_names())
    },
    #' @description a pre-computed table used internally by package functions
    obj_internal_tbl = function() {
      private$internal_tbl
    },
    #' @description
    #' Print collection object info to console.
    #' @template canonical-arg
    #' @param n Maximum number of component theme names to print to console.
    #' This defaults to NULL which means the
    #' \code{getOption("stoRy.print_min")} value is used. Run
    #' \code{options(stoRy.print_min = 25L)} to set the minimum number of
    #' printed component theme names to be 25. Run
    #' \code{stoRy_opt("print_max")} to check the maximum number of themes
    #' that can be printed to console. This value can be changed in the same
    #' way as with \code{stoRy.print_min}.
    #' @template width-arg
    print = function(canonical = FALSE,
                     n = NULL,
                     width = NULL,
                     ...) {
      # Set a custom maximum column width for printed output, if desired
      if (is.null(width)) width <- stoRy_opt("width")

      # Retrieve themeset object contents to facilitate printing to console
      themeset_id <- self$themeset_id()
      description <- self$description()
      component_theme_names <- self$component_theme_names() %>%
        pull(.data$component_theme_names)
      size <- self$size()
      max_component_theme_names <- get_number_of_printed_entries(n, number_of_entries = size)
      number_of_unprinted_entries <- size - max_component_theme_names

      # The header format does not depend on the `canonical` value
      lto_version_tag <- which_lto()
      header <- str_glue("A themeset consisting of {size} component themes")
      comment <- format_comment(header, width = min(width, stoRy_opt("width")))
      cat(comment, sep = "\n")
      
      # Print themeset info to console
      if (canonical) {
        # Print themeset ID
        cat(str_glue("{themeset_id}\n"), sep = "\n")
        cat(str_glue("{get_underline(string=themeset_id)}\n"), sep = "\n")

        # Print themeset definition
        cat("\n:: Description", sep = "\n")
        cat(str_wrap(description, width = width), sep = "\n")

        # Print up to `n` member theme names
        if (isTRUE(size > 0)) {
          cat("\n:: Component Themes", sep = "\n")
          cat(
            str_c(
              component_theme_names[1 : max_component_theme_names]
            ),
            sep = "\n"
          )
        }
      } else {
        # Print themeset ID
        cat(
          str_wrap2(
            string1 = "Themeset ID",
            string2 = themeset_id,
            width = width,
            exdent = 2
          ),
          sep = "\n"
        )

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

        # Print up to `n` member theme names
        if (isTRUE(size > 0)) {
          cat(str_glue(black$bold("Component Theme Names"), ":\n"), sep = "\n")
          
          for (i in seq.int(max_component_theme_names)) {
            cat(
              str_wrap(
                component_theme_names[i],
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
        footer <- pre_dots(str_glue("with {number_of_unprinted_entries} more component themes"))
        comment <- format_comment(footer, width = min(width, stoRy_opt("width")))
        cat(comment, sep = "\n")
      }
    }
  ),
  private = list(
    themeset = NULL
  )
)




