test_that("overwrite_action = 'unique' works correctly", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.overwrite_action = "unique"))

  filename <- "unique_test.png"

  # First save
  ggsave(filename, p)
  expect_true(file.exists(filename))

  # Second save should create a new file and print a message
  expect_message(
    ggsave(filename, p),
    "File exists. Saving to unique_test-1.png instead."
  )

  # Check that the new file exists
  expect_true(file.exists("unique_test-1.png"))
})

test_that("overwrite_action = 'stop' works correctly", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.overwrite_action = "stop"))

  filename <- "stop_test.png"
  ggsave(filename, p) # First save

  # Second save should throw an error
  expect_error(
    ggsave(filename, p),
    "File 'stop_test.png' already exists."
  )
})

test_that("overwrite_action = 'overwrite' works correctly (default)", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.overwrite_action = "overwrite"))

  filename <- "overwrite_test.png"
  ggsave(filename, p)
  info1 <- file.info(filename)

  # Wait a moment to ensure modification time can change
  Sys.sleep(0.1)

  ggsave(filename, p)
  info2 <- file.info(filename)

  # File should be overwritten (mod time changes), no new file created
  expect_true(info2$mtime > info1$mtime)
  expect_false(file.exists("overwrite_test-1.png"))
})
