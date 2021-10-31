test_that("invalid story ID generates error", {
  story_id <- "this is not a valid story ID"
  expect_error(Story$new(story_id))
})
