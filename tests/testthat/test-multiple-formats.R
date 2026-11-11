test_that("ggsaveR.formats saves one file per format and ignores the extension", {
  local_test_env(ggsaveR.formats = list(
    list(device = "png"),
    list(device = "pdf"),
    list(device = "jpeg")
  ))

  saved <- save_quietly("plot.foo", p)

  expect_equal(saved, c("plot.png", "plot.pdf", "plot.jpeg"))
  expect_setequal(list.files(), saved)
})

test_that("ggsaveR.formats can be a character vector of devices", {
  local_test_env(ggsaveR.formats = c("png", "pdf"))
  saved <- save_quietly("plot.png", p)
  expect_equal(saved, c("plot.png", "plot.pdf"))
  expect_true(all(file.exists(saved)))
})

test_that("svg output works through ggsaveR.formats", {
  skip_if_not_installed("svglite")
  local_test_env(ggsaveR.formats = c("svg"))
  expect_equal(save_quietly("plot.png", p), "plot.svg")
  expect_true(file.exists("plot.svg"))
})

test_that("each format gets its own dimensions", {
  local_test_env(ggsaveR.formats = list(
    list(device = "png", width = 8, height = 6, units = "in", dpi = 72),
    list(device = "pdf", width = 18, height = 12, units = "cm"),
    list(device = "jpeg", width = 800, height = 600, units = "px"),
    list(device = "tiff", width = 2, height = 2, units = "in", dpi = 100)
  ))

  dir.create("figs")
  saved <- suppressMessages(ggsave("figs/multi", p))

  expect_true(all(file.exists(saved)))
  expect_equal(basename(saved), paste0("multi.", c("png", "pdf", "jpeg", "tiff")))
  expect_equal(png_info(saved[1])$dim, c(8 * 72, 6 * 72))
})

test_that("format settings take precedence over arguments in the call", {
  local_test_env(ggsaveR.formats = list(
    list(device = "png", width = 4, height = 3, dpi = 100)
  ))

  # `width` is given twice; this used to fail with "matched by multiple
  # actual arguments"
  suppressMessages(ggsave("clash.png", p, width = 10, height = 10, units = "in"))
  expect_equal(png_info("clash.png")$dim, c(400, 300))
})

test_that("arguments in the call fill in what a format does not set", {
  local_test_env(ggsaveR.formats = list(list(device = "png", dpi = 100)))
  suppressMessages(ggsave("fill.png", p, width = 2, height = 1))
  expect_equal(png_info("fill.png")$dim, c(200, 100))
})

test_that("ggsaveR.formats takes precedence over the device argument", {
  local_test_env(ggsaveR.formats = list(list(device = "jpeg")))
  saved <- save_quietly("precedence", p, device = c("png", "pdf"))
  expect_equal(saved, "precedence.jpeg")
  expect_equal(list.files(), "precedence.jpeg")
})

test_that("a vector of devices saves one file per device", {
  local_test_env()
  saved <- save_quietly("plot_via_device_arg", p, device = c("png", "pdf"))
  expect_equal(saved, paste0("plot_via_device_arg", c(".png", ".pdf")))
  expect_true(all(file.exists(saved)))
})

test_that("formats without a device are skipped with a warning", {
  local_test_env(ggsaveR.formats = list(list(dpi = 300), list(device = "png")))
  expect_warning(saved <- save_quietly("skip.png", p), "missing a 'device'")
  expect_equal(saved, "skip.png")

  withr::local_options(ggsaveR.formats = list(list(dpi = 300)))
  expect_error(suppressWarnings(save_quietly("none.png", p)), "any usable format")
})

test_that("metadata is added to the PNG among several formats", {
  local_test_env(ggsaveR.formats = c("png", "pdf"), ggsaveR.embed_data = TRUE)
  save_quietly("both", p)
  expect_equal(read_ggsaveR_data("both.png")$plot_call, "p")
  expect_true(file.exists("both.pdf"))
})
