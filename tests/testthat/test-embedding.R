test_that("data embedding round-trips through a PNG", {
  local_test_env(ggsaveR.embed_data = TRUE)

  expect_message(
    ggsave("embed.png", p, width = 3, height = 2, dpi = 50),
    "Embedded reproducibility data into embed.png"
  )

  info <- read_ggsaveR_data("embed.png")
  expect_type(info, "list")
  expect_named(info, c("ggsaveR_version", "plot_object", "plot_data", "session_info", "plot_call"))

  expect_s3_class(info$plot_object, "ggplot")
  expect_equal(info$plot_data, mtcars)
  expect_s3_class(info$session_info, "sessionInfo")
  expect_equal(info$plot_call, "p")
  expect_equal(info$ggsaveR_version, utils::packageVersion("ggsaveR"))
})

test_that("the embedded plot object can be drawn again", {
  local_test_env(ggsaveR.embed_data = TRUE)
  save_quietly("embed.png", p)
  restored <- read_ggsaveR_data("embed.png")$plot_object
  expect_no_error(ggplot2::ggplot_build(restored))
})

test_that("call embedding captures inline and piped plot expressions", {
  local_test_env(ggsaveR.embed_data = TRUE)

  save_quietly(
    "inline.png",
    plot = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) + ggplot2::geom_point()
  )
  expect_match(read_ggsaveR_data("inline.png")$plot_call, "ggplot2::ggplot(iris", fixed = TRUE)

  (ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) + ggplot2::geom_point()) |>
    save_quietly(filename = "piped.png")
  expect_true(file.exists("piped.png"))
})

test_that("a plot passed by value is not deparsed into the call", {
  local_test_env(ggsaveR.embed_data = TRUE)
  suppressMessages(do.call(ggsave, list("docall.png", p, width = 3, height = 2, dpi = 50)))
  expect_equal(read_ggsaveR_data("docall.png")$plot_call, "<plot object>")
})

test_that("embed_metadata option selects what is embedded", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = c("plot", "call"))
  save_quietly("subset.png", p)
  expect_named(read_ggsaveR_data("subset.png"), c("ggsaveR_version", "plot_object", "plot_call"))
})

test_that("nothing is embedded when embed_metadata is empty", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = character())
  msgs <- capture_messages(ggsave("empty.png", p, width = 3, height = 2, dpi = 50))
  expect_false(any(grepl("Embedded reproducibility data", msgs)))
  expect_null(png_metadata("empty.png")$ggsaveR_data)
})

test_that("embedding is skipped for non-PNG files", {
  local_test_env(ggsaveR.embed_data = TRUE)

  msgs <- capture_messages(ggsave("no_embed.pdf", p, width = 3, height = 2))
  expect_false(any(grepl("Embedded reproducibility data", msgs)))
  expect_true(file.exists("no_embed.pdf"))
})

test_that("read_ggsaveR_data() validates its input", {
  local_test_env()
  expect_error(read_ggsaveR_data("does_not_exist.png"), "File not found")
  expect_error(read_ggsaveR_data(1), "single string")

  save_quietly("plot.pdf", p)
  expect_error(read_ggsaveR_data("plot.pdf"), "only supported for PNG files")
})

test_that("read_ggsaveR_data() warns and returns NULL when there is nothing to read", {
  local_test_env()

  save_quietly("plain.png", p)
  expect_warning(res <- read_ggsaveR_data("plain.png"), "No ggsaveR data found")
  expect_null(res)

  writeLines("this is not a PNG", "broken.png")
  expect_warning(res <- read_ggsaveR_data("broken.png"), "Could not read ggsaveR data")
  expect_null(res)
})

test_that("read_ggsaveR_data() handles a corrupt payload", {
  local_test_env()
  img <- array(1, c(2, 2, 3))
  png::writePNG(img, "corrupt.png", metadata = list(ggsaveR_data = "bm90IGd6aXA="))
  expect_warning(res <- read_ggsaveR_data("corrupt.png"), "Could not read ggsaveR data")
  expect_null(res)
})
