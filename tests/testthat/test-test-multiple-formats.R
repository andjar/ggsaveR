test_that("ggsave can save to multiple formats at once", {
  withr::local_dir(tempdir())

  base_filename <- "multi_format_plot"

  # Save to two formats
  saved_files <- ggsave(base_filename, p, device = c("png", "pdf"))

  # Check that both files were created
  expected_png <- paste0(base_filename, ".png")
  expected_pdf <- paste0(base_filename, ".pdf")

  expect_true(file.exists(expected_png))
  expect_true(file.exists(expected_pdf))

  # Check that the function returns the paths of the created files
  expect_equal(sort(saved_files), sort(c(expected_png, expected_pdf)))
})
