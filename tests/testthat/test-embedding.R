test_that("data embedding round-trips through a PNG", {
  local_test_env(ggsaveR.embed_data = TRUE)

  expect_message(
    ggsave("embed.png", p, width = 3, height = 2, dpi = 50),
    "Embedded reproducibility data into embed.png"
  )

  expect_message(read_ggsaveR_data("embed.png"), "trusted = TRUE")
  info <- suppressMessages(read_ggsaveR_data("embed.png"))
  expect_type(info, "list")
  expect_named(info, c("ggsaveR_version", "plot_call", "session_info", "plot_data"))
  expect_equal(info$ggsaveR_version, utils::packageVersion("ggsaveR"))
  expect_equal(info$plot_call, "p")
  expect_match(info$session_info, "Platform:")
  expect_equal(info$plot_data, mtcars, ignore_attr = TRUE)
})

test_that("the plot object is only restored for trusted files", {
  local_test_env(ggsaveR.embed_data = TRUE)
  save_quietly("embed.png", p)

  untrusted <- suppressMessages(read_ggsaveR_data("embed.png"))
  expect_null(untrusted$plot_object)

  trusted <- read_ggsaveR_data("embed.png", trusted = TRUE)
  expect_named(trusted, c("ggsaveR_version", "plot_call", "session_info", "plot_data", "plot_object"))
  expect_s3_class(trusted$plot_object, "ggplot")
  expect_equal(trusted$plot_object$data, mtcars)
  expect_no_error(ggplot2::ggplot_build(trusted$plot_object))

  expect_error(read_ggsaveR_data("embed.png", trusted = NA), "`trusted`")
})

test_that("call, session and data are stored without serialization", {
  local_test_env(ggsaveR.embed_data = TRUE)
  save_quietly("embed.png", p)
  meta <- png_metadata("embed.png")

  expect_equal(meta$ggsaveR_call, "p")
  expect_match(meta$ggsaveR_session, "Platform:")
  csv <- rawToChar(memDecompress(base64enc::base64decode(meta$ggsaveR_data_csv), "gzip"))
  expect_match(csv, '^"mpg","cyl"')
})

test_that("non-ASCII data survive the round trip", {
  skip_if_not(l10n_info()$`UTF-8`, "needs a UTF-8 locale")
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = "data")
  df <- data.frame(x = 1:2, y = 1:2, by = c("Tromsø", "Ålesund"))
  save_quietly("utf8.png", ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point())
  expect_equal(read_ggsaveR_data("utf8.png")$plot_data$by, c("Tromsø", "Ålesund"))
})

test_that("data that cannot be written as CSV are skipped with a warning", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = c("data", "call"))
  df <- data.frame(x = 1:2, y = 1:2)
  df$l <- list(1, "a")
  pl <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_warning(save_quietly("list.png", pl), "could not be written as CSV")
  expect_named(read_ggsaveR_data("list.png"), c("ggsaveR_version", "plot_call"))
})

test_that("large payloads are left out with a warning", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_max_size = 100)
  expect_warning(save_quietly("big.png", p), "ggsaveR.embed_max_size")
  info <- read_ggsaveR_data("big.png", trusted = TRUE)
  expect_named(info, c("ggsaveR_version", "plot_call", "session_info"))
})

test_that("call embedding captures inline and piped plot expressions", {
  local_test_env(ggsaveR.embed_data = TRUE)

  save_quietly(
    "inline.png",
    plot = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) + ggplot2::geom_point()
  )
  expect_match(
    suppressMessages(read_ggsaveR_data("inline.png"))$plot_call,
    "ggplot2::ggplot(iris", fixed = TRUE
  )

  (ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) + ggplot2::geom_point()) |>
    save_quietly(filename = "piped.png")
  expect_true(file.exists("piped.png"))
})

test_that("a plot passed by value is not deparsed into the call", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = "call")
  suppressMessages(do.call(ggsave, list("docall.png", p, width = 3, height = 2, dpi = 50)))
  expect_equal(read_ggsaveR_data("docall.png")$plot_call, "<plot object>")
})

test_that("embed_metadata option selects what is embedded", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = c("plot", "call"))
  save_quietly("subset.png", p)
  expect_named(
    read_ggsaveR_data("subset.png", trusted = TRUE),
    c("ggsaveR_version", "plot_call", "plot_object")
  )
})

test_that("only the version is embedded when embed_metadata is empty", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.embed_metadata = character())
  save_quietly("empty.png", p)
  expect_named(read_ggsaveR_data("empty.png"), "ggsaveR_version")
})

test_that("embedding is skipped for non-PNG files", {
  local_test_env(ggsaveR.embed_data = TRUE)

  msgs <- capture_messages(ggsave("no_embed.pdf", p, width = 3, height = 2))
  expect_false(any(grepl("Embedded reproducibility data", msgs)))
  expect_true(file.exists("no_embed.pdf"))
})

test_that("files from ggsaveR 0.1.0 can be read when trusted", {
  local_test_env()
  legacy <- list(ggsaveR_version = package_version("0.1.0"), plot_call = "p")
  png::writePNG(array(1, c(2, 2, 3)), "legacy.png",
                metadata = list(ggsaveR_data = ggsaveR:::encode_object(legacy)))

  expect_warning(res <- read_ggsaveR_data("legacy.png"), "older version")
  expect_null(res)
  expect_equal(read_ggsaveR_data("legacy.png", trusted = TRUE), legacy)
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
  png::writePNG(img, "corrupt.png",
                metadata = list(ggsaveR_version = "0.2.0", ggsaveR_data_csv = "bm90IGd6aXA="))
  expect_warning(res <- read_ggsaveR_data("corrupt.png"), "Could not read ggsaveR data")
  expect_null(res)
})
