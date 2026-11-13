test_that("ggsave works with default settings and passes '...' arguments", {
  local_test_env()

  out <- suppressMessages(ggsave("test_core.png", p))
  expect_true(file.exists("test_core.png"))
  expect_equal(out, "test_core.png")

  suppressMessages(ggsave("test_dims.png", p, width = 4, height = 3, units = "in", dpi = 100))
  expect_equal(png_info("test_dims.png")$dim, c(400, 300))
})

test_that("the return value is invisible", {
  local_test_env()
  expect_invisible(save_quietly("invisible.png", p))
})

test_that("the last plot is saved by default", {
  local_test_env()
  ggplot2::set_last_plot(p)
  save_quietly("last.png")
  expect_true(file.exists("last.png"))
})

test_that("device can be a function, as in ggplot2", {
  local_test_env()
  out <- save_quietly("fn_device.png", p, device = grDevices::png)
  expect_equal(out, "fn_device.png")
  expect_true(file.exists("fn_device.png"))
})

test_that("the file name is kept as given when it has an extension", {
  local_test_env()

  expect_equal(save_quietly("upper.PNG", p), "upper.PNG")
  expect_true("upper.PNG" %in% list.files())

  expect_equal(save_quietly("my.plot", p, device = "png"), "my.plot")
  expect_true("my.plot" %in% list.files())
})

test_that("a single device is appended to a file name without extension", {
  local_test_env()
  expect_equal(save_quietly("noext", p, device = "pdf"), "noext.pdf")
  expect_true(file.exists("noext.pdf"))
})

test_that("non-ggplot grobs can be saved", {
  local_test_env(ggsaveR.embed_data = TRUE)
  save_quietly("grob.png", grid::rectGrob())
  expect_true(file.exists("grob.png"))
  expect_s3_class(read_ggsaveR_data("grob.png", trusted = TRUE)$plot_object, "grob")
})

test_that("the path argument is combined with filename, as in ggplot2", {
  local_test_env(ggsaveR.overwrite_action = "stop")
  dir.create("sub")
  expect_equal(save_quietly("plot.png", p, path = "sub"), file.path("sub", "plot.png"))
  expect_true(file.exists("sub/plot.png"))
  # The overwrite check sees the real location
  expect_error(save_quietly("plot.png", p, path = "sub"), "already exists")
})

test_that("unusable input gives informative errors", {
  local_test_env()
  expect_error(save_quietly("no_device", p), "Cannot determine device")
  expect_error(save_quietly(c("a.png", "b.png"), p), "single string")
  expect_error(save_quietly(p), "single string")
})

test_that("invalid options are reported instead of silently ignored", {
  local_test_env()
  withr::local_options(ggsaveR.overwrite_action = "uniqe")
  expect_error(save_quietly("a.png", p), "ggsaveR.overwrite_action")

  withr::local_options(ggsaveR.overwrite_action = "overwrite", ggsaveR.embed_data = "yes")
  expect_error(save_quietly("a.png", p), "ggsaveR.embed_data")

  withr::local_options(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = c("plot", "code"))
  expect_error(save_quietly("a.png", p), "ggsaveR.embed_metadata")

  withr::local_options(ggsaveR.embed_metadata = "plot", ggsaveR.creator = 1)
  expect_error(save_quietly("a.png", p), "ggsaveR.creator")

  withr::local_options(ggsaveR.creator = NULL, ggsaveR.formats = 1:2)
  expect_error(save_quietly("a.png", p), "ggsaveR.formats")

  withr::local_options(ggsaveR.formats = list(list(device = grDevices::png)))
  expect_error(save_quietly("a.png", p), "as strings")

  withr::local_options(ggsaveR.formats = list(list(device = "png", suffix = 1)))
  expect_error(save_quietly("a.png", p), "suffix")

  withr::local_options(ggsaveR.formats = NULL, ggsaveR.size_suffix = "yes")
  expect_error(save_quietly("a.png", p), "ggsaveR.size_suffix")

  withr::local_options(ggsaveR.size_suffix = "auto", ggsaveR.dir = 1)
  expect_error(save_quietly("a.png", p), "ggsaveR.dir")

  withr::local_options(ggsaveR.dir = NULL, ggsaveR.variants = "sepia")
  expect_error(save_quietly("a.png", p), "ggsaveR.variants")

  withr::local_options(ggsaveR.variants = NULL, ggsaveR.embed_max_size = -1)
  expect_error(save_quietly("a.png", p), "ggsaveR.embed_max_size")

  withr::local_options(ggsaveR.embed_max_size = 5e6, ggsaveR.log = TRUE)
  expect_error(save_quietly("a.png", p), "ggsaveR.log")
  expect_false(file.exists("a.png"))
})

test_that("default options are set when the package is loaded", {
  withr::local_options(list(
    ggsaveR.overwrite_action = NULL,
    ggsaveR.size_suffix = NULL,
    ggsaveR.embed_data = NULL,
    ggsaveR.embed_metadata = NULL
  ))
  ggsaveR:::.onLoad()
  expect_equal(getOption("ggsaveR.overwrite_action"), "overwrite")
  expect_equal(getOption("ggsaveR.size_suffix"), "auto")
  expect_false(getOption("ggsaveR.embed_data"))
  expect_equal(getOption("ggsaveR.embed_metadata"), c("plot", "data", "session_info", "call"))
})

test_that(".onLoad does not override options the user has already set", {
  withr::local_options(list(ggsaveR.overwrite_action = "stop"))
  ggsaveR:::.onLoad()
  expect_equal(getOption("ggsaveR.overwrite_action"), "stop")
})
