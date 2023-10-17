test_that("initialization from string works", {
  file <- I("Collection: Three Miscellaneous Twilight Zone Episodes
======================================================

:: Title
Three Miscellaneous Twilight Zone Episodes
  
:: Date
1959-1964
  
:: Description
Three miscellaneous Twilight Zone episodes used for testing purposes.

:: References
https://www.themeontology.org
  
:: Collections
Collection: Three Miscellaneous Twilight Zone Episodes

:: Component Stories
tz1959e3x24
tz1959e1x22
tz1959e2x06")
  collection_id <- "Collection: Three Miscellaneous Twilight Zone Episodes"
  date <- "1959-1964"
  description <- "Three miscellaneous Twilight Zone episodes used for testing purposes."
  references <- "https://www.themeontology.org"
  component_story_ids <- as_tibble_col(c("tz1959e3x24", "tz1959e1x22", "tz1959e2x06"), column_name = "component_story_ids")
  collection <- Collection$new(collection_id, file)
  expect_equal(collection$component_story_ids(), component_story_ids)
})
