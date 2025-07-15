test_that("ggsave works with default settings and passes '...' arguments", {
  # Use a temporary directory for test files
  withr::local_dir(tempdir())

  # Ensure options are at their default for this test
  withr::local_options(list(
    ggsaveR.overwrite_action = "overwrite",
    ggsaveR.embed_data = FALSE,
    ggsaveR.creator = NULL
  ))

  filename <- "test_core.png"

  # Test basic saving
  ggsave(filename, p)
  expect_true(file.exists(filename))

  # Test that '...' arguments like width and height are passed
  ggsave("test_dims.png", p, width = 4, height = 3, units = "in", dpi = 100)
  img <- png::readPNG("test_dims.png")

  # Dimensions should be width * dpi and height * dpi
  expect_equal(dim(img)[2], 400) # width
  expect_equal(dim(img)[1], 300) # height
})
