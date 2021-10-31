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
  collection_id <- unlist(strsplit(file, split = "\n"))[1]
  collection <- Collection$new(collection_id, file)
  component_story_ids <- c("tz1959e3x24", "tz1959e1x22", "tz1959e2x06")
  expect_equal(pull(collection$component_story_ids()), component_story_ids)
})
