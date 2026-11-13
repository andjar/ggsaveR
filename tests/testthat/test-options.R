test_that("ggsaveR_options() sets options and returns the old values", {
  local_test_env()
  old <- ggsaveR_options(formats = c("png", "pdf"), overwrite_action = "unique")
  expect_equal(getOption("ggsaveR.formats"), c("png", "pdf"))
  expect_equal(getOption("ggsaveR.overwrite_action"), "unique")
  expect_equal(old, list(ggsaveR.formats = NULL, ggsaveR.overwrite_action = "overwrite"))

  options(old)
  expect_null(getOption("ggsaveR.formats"))
  expect_equal(getOption("ggsaveR.overwrite_action"), "overwrite")
})

test_that("ggsaveR_options() validates and leaves options unchanged on error", {
  local_test_env()
  expect_error(ggsaveR_options(overwrite_action = "uniqe"), "ggsaveR.overwrite_action")
  expect_equal(getOption("ggsaveR.overwrite_action"), "overwrite")
  expect_error(ggsaveR_options(formatz = "png"), "unused argument")
})

test_that("ggsaveR_options() can reset an option with NULL", {
  local_test_env(ggsaveR.creator = "Me")
  ggsaveR_options(creator = NULL)
  expect_null(getOption("ggsaveR.creator"))
})

test_that("ggsaveR_options() without arguments lists all options", {
  local_test_env(ggsaveR.creator = "Me")
  all <- ggsaveR_options()
  expect_length(all, 10)
  expect_true(all(startsWith(names(all), "ggsaveR.")))
  expect_equal(all$ggsaveR.creator, "Me")
})
