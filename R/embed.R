#' Read reproducibility data embedded in a PNG
#'
#' Extracts the data that [ggsave()] embeds in a PNG file when the
#' `ggsaveR.embed_data` option is `TRUE`.
#'
#' @section Security:
#' The data are stored as a serialized R object and restored with
#' [unserialize()]. Unserializing a file from an untrusted source can execute
#' arbitrary code, so only use this function on PNG files that you, or someone
#' you trust, created.
#'
#' @param path Path to a PNG file written by [ggsave()].
#' @return A list with some or all of the elements `ggsaveR_version`,
#'   `plot_object`, `plot_data`, `session_info` and `plot_call`, depending on
#'   the `ggsaveR.embed_metadata` option in effect when the file was saved.
#'   Returns `NULL`, with a warning, if the file holds no ggsaveR data or
#'   cannot be read.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' out <- file.path(tempdir(), "embedded.png")
#'
#' old <- options(ggsaveR.embed_data = TRUE)
#' ggsave(out, p, width = 5, height = 4)
#' options(old)
#'
#' info <- read_ggsaveR_data(out)
#' names(info)
#' info$plot_call
#' head(info$plot_data)
#' @export
read_ggsaveR_data <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (tolower(tools::file_ext(path)) != "png") {
    stop("Data embedding is only supported for PNG files.", call. = FALSE)
  }

  tryCatch({
    # info = TRUE also returns the text chunks as the "metadata" attribute
    png_info <- png::readPNG(path, info = TRUE)
    encoded <- attr(png_info, "metadata", exact = TRUE)$ggsaveR_data

    if (is.null(encoded)) {
      warning("No ggsaveR data found in '", path, "'.", call. = FALSE)
      return(NULL)
    }
    decode_payload(encoded)
  }, error = function(e) {
    warning(
      "Could not read ggsaveR data from '", path, "': ", conditionMessage(e),
      call. = FALSE
    )
    NULL
  })
}

# R object -> serialize -> gzip -> base64, so that it fits in a PNG tEXt chunk
encode_payload <- function(x) {
  base64enc::base64encode(memCompress(serialize(x, NULL), type = "gzip"))
}

decode_payload <- function(x) {
  unserialize(memDecompress(base64enc::base64decode(x), type = "gzip"))
}
