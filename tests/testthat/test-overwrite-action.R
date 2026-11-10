test_that("overwrite_action = 'unique' never overwrites", {
  local_test_env(ggsaveR.overwrite_action = "unique")

  expect_equal(save_quietly("plot.png", p), "plot.png")
  expect_message(
    second <- ggsave("plot.png", p, width = 3, height = 2, dpi = 50),
    "File exists. Saving to plot-1.png instead."
  )
  expect_equal(second, "plot-1.png")
  expect_equal(save_quietly("plot.png", p), "plot-2.png")
  expect_setequal(list.files(), c("plot.png", "plot-1.png", "plot-2.png"))
})

test_that("'unique' keeps the directory and gives all formats the same suffix", {
  local_test_env(ggsaveR.overwrite_action = "unique", ggsaveR.formats = c("png", "pdf"))
  dir.create("figs")
  file.create("figs/plot.pdf") # only the PDF exists already

  saved <- save_quietly("figs/plot.png", p)
  expect_equal(saved, c("figs/plot-1.png", "figs/plot-1.pdf"))
  expect_true(all(file.exists(saved)))
  expect_false(file.exists("figs/plot.png"))
})

test_that("overwrite_action = 'stop' errors when the file exists", {
  local_test_env(ggsaveR.overwrite_action = "stop")

  save_quietly("plot.png", p)
  expect_error(save_quietly("plot.png", p), "File 'plot.png' already exists.", fixed = TRUE)
})

test_that("'stop' fails before any file of a multi-format save is written", {
  local_test_env(ggsaveR.overwrite_action = "stop", ggsaveR.formats = c("png", "pdf"))
  file.create("plot.pdf")

  expect_error(save_quietly("plot.png", p), "'plot.pdf' already exists")
  expect_false(file.exists("plot.png"))
})

test_that("overwrite_action = 'overwrite' replaces the file", {
  local_test_env()
  writeLines("placeholder", "plot.png")

  save_quietly("plot.png", p)

  expect_equal(list.files(), "plot.png")
  expect_equal(png_info("plot.png")$dim, c(150, 100))
})

test_that("unique_filename() finds free names", {
  local_test_env()

  expect_equal(unique_filename("new.png"), "new.png")

  file.create(c("test.png", "noext", "archive.tar.gz"))
  expect_equal(suppressMessages(unique_filename("test.png")), "test-1.png")
  expect_equal(suppressMessages(unique_filename("noext")), "noext-1")
  expect_equal(suppressMessages(unique_filename("archive.tar.gz")), "archive.tar-1.gz")

  file.create("test-1.png")
  expect_equal(suppressMessages(unique_filename("test.png")), "test-2.png")
})
