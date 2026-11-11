test_that("creator and standard metadata are added to PNG files", {
  local_test_env(ggsaveR.creator = "Test Author, ggsaveR")
  save_quietly("creator.png", p)

  meta <- png_metadata("creator.png")
  expect_equal(meta$Author, "Test Author, ggsaveR")
  expect_equal(meta$Description, "Plot generated from R code: p")
  expect_match(meta$Software, "ggsaveR")
  expect_false(is.null(meta[["Creation Time"]]))
  # No reproducibility payload unless embed_data is on
  expect_null(meta$ggsaveR_data)
})

test_that("PNGs are left untouched when no metadata is requested", {
  local_test_env()
  save_quietly("plain.png", p)
  expect_null(png_metadata("plain.png")$Software)
})

test_that("non-PNG formats are saved normally when a creator is set", {
  local_test_env(ggsaveR.creator = "Test Author")
  expect_no_error(save_quietly("creator.pdf", p))
  expect_true(file.exists("creator.pdf"))
})

test_that("PNGs with metadata are rendered exactly like plain ggsave() output", {
  local_test_env()
  args <- list(plot = p, width = 4, height = 3, units = "in", dpi = 60, scale = 2, bg = "grey90")

  suppressMessages(do.call(ggplot2::ggsave, c(list(filename = "reference.png"), args)))
  withr::local_options(ggsaveR.creator = "Test Author")
  suppressMessages(do.call(ggsave, c(list(filename = "with_meta.png"), args)))

  ref <- png::readPNG("reference.png", info = TRUE)
  out <- png::readPNG("with_meta.png", info = TRUE)

  # scale = 2 doubles the canvas: 4 in * 2 * 60 dpi
  expect_equal(attr(out, "info")$dim, c(480, 360))
  expect_equal(attr(out, "info")$dpi, attr(ref, "info")$dpi, tolerance = 1e-3)
  expect_equal(as.vector(out), as.vector(ref))
})

test_that("string dpi values work together with metadata", {
  local_test_env(ggsaveR.creator = "Test Author")
  suppressMessages(ggsave("screen.png", p, width = 2, height = 2, dpi = "screen"))
  expect_equal(png_info("screen.png")$dim, c(144, 144))
})

test_that("pixel units work together with metadata", {
  local_test_env(ggsaveR.creator = "Test Author")
  suppressMessages(ggsave("px.png", p, width = 300, height = 200, units = "px"))
  expect_equal(png_info("px.png")$dim, c(300, 200))
})

test_that("a missing target directory is an error unless create.dir = TRUE", {
  local_test_env(ggsaveR.creator = "Test Author")
  expect_error(save_quietly("missing/plot.png", p), "Cannot find directory")
  expect_false(dir.exists("missing"))

  skip_if(utils::packageVersion("ggplot2") < "3.5.0", "create.dir needs ggplot2 >= 3.5.0")
  save_quietly("created/plot.png", p, create.dir = TRUE)
  expect_true(file.exists("created/plot.png"))
})
