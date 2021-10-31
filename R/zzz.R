.onAttach <- function(...) {
  if (!interactive()) return()

  tip <- random_tip()
  packageStartupMessage(str_glue("Loading stoRy {utils::packageVersion(\"stoRy\")}"))
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}

random_tip <- function() {
  tips <- c(
    "Explore the themes at https://themeontology.org/themes",
    "Peruse the thematically annotated stories at https://themeontology.org/stories",
    "Visit the Theme Ontology GitHub page at https://github.com/theme-ontology",
    "Learn more about the underlying theory at https://github.com/theme-ontology/theming",
    "Use suppressPackageStartupMessages() to eliminate package startup messages",
    "Need help getting started? Try the stoRy vignette: https://cran.r-project.org/web/packages/stoRy/vignettes/vignette.pdf",
    "Want to get the gist of what the Theme Ontology is all about? Try the slides: https://pdfs.semanticscholar.org/4bd7/7c918e339593cb2a3fd2ccbfbf0f598db11e.pdf",
    "Want to understand how all the pieces fit together? Read The Literary Theme Ontology for Media Annotation and Information Retrieval: http://ceur-ws.org/Vol-2518/paper-WODHSA8.pdf"
  )

  sample(tips, 1)
}
