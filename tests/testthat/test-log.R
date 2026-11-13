test_that("every saved file is appended to the log", {
  local_test_env(ggsaveR.log = "logs/figures.csv", ggsaveR.formats = c("png", "pdf"))
  save_quietly("fig.png", p)
  save_quietly("other.png", p + ggplot2::theme_bw())

  log <- utils::read.csv("logs/figures.csv", stringsAsFactors = FALSE)
  expect_equal(nrow(log), 4)
  expect_named(log, c("time", "file", "device", "width", "height", "units",
                      "dpi", "bytes", "md5", "plot"))
  expect_equal(log$file, c("fig.png", "fig.pdf", "other.png", "other.pdf"))
  expect_equal(log$device, c("png", "pdf", "png", "pdf"))
  expect_equal(log$width, rep(3, 4))
  expect_equal(log$dpi, c(50, NA, 50, NA))
  expect_equal(log$md5[1], unname(tools::md5sum("fig.png")))
  expect_equal(log$plot, c("p", "p", "p + ggplot2::theme_bw()", "p + ggplot2::theme_bw()"))
})

test_that("variants are logged, and multi-line calls fit on one line", {
  local_test_env(ggsaveR.log = "log.csv", ggsaveR.variants = "greyscale")
  save_quietly("fig.png", ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::labs(title = "A rather long title that forces the deparsed call onto two lines"))
  log <- utils::read.csv("log.csv", stringsAsFactors = FALSE)
  expect_equal(log$file, c("fig.png", "fig_greyscale.png"))
  expect_false(any(grepl("\n", log$plot)))
})

test_that("nothing is logged by default or with guard = TRUE", {
  local_test_env()
  save_quietly("fig.png", p)
  withr::local_options(ggsaveR.log = "log.csv")
  save_quietly("guarded.png", p, guard = TRUE)
  expect_false(file.exists("log.csv"))
})
