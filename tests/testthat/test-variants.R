p_col <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour = factor(cyl))) +
  ggplot2::geom_point(size = 3)

test_that("a greyscale variant is saved next to the plot", {
  local_test_env(ggsaveR.variants = "greyscale")
  saved <- save_quietly("fig.png", p_col)
  expect_equal(saved, c("fig.png", "fig_greyscale.png"))

  orig <- png::readPNG("fig.png")
  grey <- png::readPNG("fig_greyscale.png", info = TRUE)
  expect_equal(dim(grey), dim(orig))
  expect_equal(attr(grey, "info")$dpi, c(50, 50), tolerance = 0.01)
  expect_equal(grey[, , 1], grey[, , 2])
  expect_equal(grey[, , 2], grey[, , 3])
})

test_that("greyscale keeps luminance: white stays white, black stays black", {
  img <- array(c(1, 0, 1, 0, 1, 0), c(1, 2, 3))
  grey <- ggsaveR:::convert_image(img, "greyscale")
  expect_equal(as.vector(grey), c(1, 0, 1, 0, 1, 0), tolerance = 1e-6)

  # Pure green is much lighter than pure blue
  img <- array(c(0, 0, 1, 0, 0, 1), c(1, 2, 3))
  grey <- ggsaveR:::convert_image(img, "greyscale")
  expect_gt(grey[1, 1, 1], grey[1, 2, 1])
})

test_that("grey images and the alpha channel are left alone", {
  grey_img <- array(0.5, c(2, 2))
  expect_identical(ggsaveR:::convert_image(grey_img, "greyscale"), grey_img)

  rgba <- array(c(rep(0.2, 4), rep(0.4, 4), rep(0.6, 4), rep(0.3, 4)), c(2, 2, 4))
  expect_equal(ggsaveR:::convert_image(rgba, "greyscale")[, , 4], rgba[, , 4])
})

test_that("colour vision deficiency variants are named after the first format", {
  skip_if_not_installed("colorspace")
  local_test_env(
    ggsaveR.formats = c("pdf", "png"),
    ggsaveR.variants = c("deutan", "protan", "tritan", "grayscale")
  )
  saved <- save_quietly("fig.png", p_col)
  expect_equal(saved, c(
    "fig.pdf", "fig.png",
    "fig_deutan.png", "fig_protan.png", "fig_tritan.png", "fig_grayscale.png"
  ))
  expect_true(all(file.exists(saved)))

  deutan <- png::readPNG("fig_deutan.png")
  expect_false(isTRUE(all.equal(deutan, png::readPNG("fig.png"))))
})

test_that("variants follow overwrite_action", {
  local_test_env(ggsaveR.variants = "greyscale", ggsaveR.overwrite_action = "stop")
  file.create("fig_greyscale.png")
  expect_error(save_quietly("fig.png", p_col), "fig_greyscale.png")
  expect_false(file.exists("fig.png"))

  withr::local_options(ggsaveR.overwrite_action = "unique")
  expect_equal(save_quietly("fig.png", p_col), c("fig-1.png", "fig_greyscale-1.png"))
})

test_that("variants ignore arguments that only the main device understands", {
  local_test_env(
    ggsaveR.variants = "greyscale",
    ggsaveR.formats = list(list(device = "pdf", width = 3, height = 2, colormodel = "srgb"))
  )
  expect_equal(save_quietly("fig.png", p_col), c("fig.pdf", "fig_greyscale.png"))
})

test_that("preview_variants() returns and draws a grid of panels", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())

  g <- preview_variants(p_col, variants = "greyscale", width = 2, height = 2, dpi = 30)
  expect_s3_class(g, "gTree")
  # One title and one image per panel
  expect_length(g$children, 4)

  skip_if_not_installed("colorspace")
  g <- preview_variants(p_col, width = 2, height = 2, dpi = 30, draw = FALSE)
  expect_length(g$children, 10)
  labels <- vapply(Filter(function(x) inherits(x, "text"), g$children), function(x) x$label, "", USE.NAMES = FALSE)
  expect_equal(labels, c("Original", "Greyscale", "Deuteranopia (deutan)",
                         "Protanopia (protan)", "Tritanopia (tritan)"))
})

test_that("the preview can itself be saved with ggsave()", {
  local_test_env()
  g <- preview_variants(p_col, variants = "greyscale", width = 2, height = 2, dpi = 30, draw = FALSE)
  save_quietly("preview.png", g)
  expect_true(file.exists("preview.png"))
})

test_that("preview_variants() validates variants", {
  expect_error(preview_variants(p_col, variants = "sepia"), "can only contain")
})

test_that("missing colorspace gives a clear error", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(ggsaveR:::check_variants("deutan"), "colorspace")
  expect_equal(ggsaveR:::check_variants("greyscale"), "greyscale")
})
