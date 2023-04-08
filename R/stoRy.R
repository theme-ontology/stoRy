#' @importFrom crayon black blue bold silver
#' @importFrom dplyr %>% arrange bind_rows case_when count desc distinct
#' @importFrom dplyr filter full_join group_by inner_join last left_join
#' @importFrom dplyr mutate nest_join pull rename relocate right_join select
#' @importFrom dplyr slice_head summarise ungroup
#' @importFrom R6 R6Class
#' @importFrom stringr str_c str_detect str_ends str_glue str_split str_starts
#' @importFrom stringr str_trim str_which str_wrap
#' @importFrom tibble add_column add_row as_tibble_col column_to_rownames
#' @importFrom tibble tibble
#' @importFrom tidyjson as_tibble as.tbl_json append_values_string
#' @importFrom tidyjson enter_object gather_array gather_object jstring 
#' @importFrom tidyjson read_json spread_all spread_values
#' @importFrom tidyr pivot_wider unnest
#' @import cli
#' @import lifecycle
#' @import rlang
#' @aliases NULL stoRy stoRy-package
#' @details
#' `r lifecycle::badge("stable")`
#'
#' The \pkg{stoRy} package provides utilities for working with \acronym{LTO}
#' data. The \acronym{LTO} is a hierarchically organized collection of
#' carefully defined "themes" that can be expected to arise in multiple
#' "stories" (i.e. works of fiction). Included in the package are functions to
#' download and cache \acronym{LTO} data, explore \acronym{LTO} themes and
#' thematically annotated stories, and analyze the thematically annotated
#' story data in interesting ways. 
#'
#' General resources:
#'   * stoRy package GitHub repository:
#'     <https://github.com/theme-ontology/stoRy>
#'   * \acronym{LTO} project website: <https://www.themeontology.org>
#'   * \acronym{LTO} project GitHub repositories: <https://github.com/theme-ontology>
#'   * [LTO conference paper](https://ceur-ws.org/Vol-2518/paper-WODHSA8.pdf)
#'     in *Proceedings of the Joint Ontology Workshops 2019 Episode V: The
#'     Styrian Autumn of Ontology*
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
