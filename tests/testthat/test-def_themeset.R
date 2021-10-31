test_that("initialization from string works", {
  file <- I("Themeset: Three Miscellaneous Themes
====================================

:: Description
A test themeset.

:: References
https://www.themeontology.org

:: Component Themes
romantic love
husband and wife
obsession")
  themeset <- Themeset$new(file)
  component_theme_names <- c("romantic love", "husband and wife", "obsession")
  expect_equal(pull(themeset$component_theme_names()), component_theme_names)
})
