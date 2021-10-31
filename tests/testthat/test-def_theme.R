test_that("invalid theme name generates error", {
  theme_name <- "this is not a valid theme name"
  expect_error(Theme$new(theme_name))
})
