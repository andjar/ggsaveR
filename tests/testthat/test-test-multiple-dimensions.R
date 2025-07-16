library(testthat)
library(ggplot2)
library(ggsaveR)

test_that("ggsave supports multiple dimensions and formats", {
  withr::local_dir(tempdir())

  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

  base_filename <- "multi_dim_plot"

  figure_output_formats <- list(
    list(filetype = "png", width = 180, height = 120, units = "mm"),
    list(filetype = "png", width = 90, height = 120, units = "mm"),
    list(filetype = "pdf", width = 180, height = 120, units = "mm"),
    list(filetype = "pdf", width = 90, height = 120, units = "mm"),
    list(filetype = "jpg", width = 180, height = 120, units = "mm"),
    list(filetype = "jpg", width = 90, height = 120, units = "mm"),
    list(filetype = "eps", width = 180, height = 120, units = "mm"),
    list(filetype = "tiff", width = 90, height = 120, units = "mm")
  )

  saved_files <- ggsave(filename = base_filename, plot = p, device = figure_output_formats)

  expected_files <- c(
    "multi_dim_plot_180x120.png",
    "multi_dim_plot_90x120.png",
    "multi_dim_plot_180x120.pdf",
    "multi_dim_plot_90x120.pdf",
    "multi_dim_plot_180x120.jpg",
    "multi_dim_plot_90x120.jpg",
    "multi_dim_plot_180x120.eps",
    "multi_dim_plot_90x120.tiff"
  )

  expect_true(all(file.exists(expected_files)))
  expect_equal(sort(saved_files), sort(expected_files))
})
