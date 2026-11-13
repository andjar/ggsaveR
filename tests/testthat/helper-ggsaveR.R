# A standard plot object to use in all tests
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
  ggplot2::geom_point()

# Run a test in an empty temporary directory with default ggsaveR options and
# without ggplot2's "Saving 7 x 7 in image" chatter.
local_test_env <- function(..., .env = parent.frame()) {
  withr::local_dir(withr::local_tempdir(.local_envir = .env), .local_envir = .env)
  withr::local_options(
    list(
      ggsaveR.formats = NULL,
      ggsaveR.size_suffix = "auto",
      ggsaveR.dir = NULL,
      ggsaveR.overwrite_action = "overwrite",
      ggsaveR.variants = NULL,
      ggsaveR.embed_data = FALSE,
      ggsaveR.embed_metadata = c("plot", "data", "session_info", "call"),
      ggsaveR.embed_max_size = 5e6,
      ggsaveR.creator = NULL,
      ggsaveR.log = NULL
    ),
    .local_envir = .env
  )
  withr::local_options(list(...), .local_envir = .env)
}

# Quiet save with small default dimensions to keep the tests fast
save_quietly <- function(...) suppressMessages(ggsave(..., width = 3, height = 2, dpi = 50))

png_metadata <- function(path) attr(png::readPNG(path, info = TRUE), "metadata")
png_info <- function(path) attr(png::readPNG(path, info = TRUE), "info")
