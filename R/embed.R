#' Build the text chunks written to a PNG
#'
#' Every part is stored in its own chunk. The call and session information are
#' plain text, the data are a CSV file (compressed and base64 encoded), and
#' only the plot object is a serialized R object. This keeps the call, data
#' and session readable without unserializing anything, from R or any other
#' language.
#' @noRd
png_text_chunks <- function(plot, plot_call, opts, filename) {
  chunks <- list()
  if (!is.null(opts$creator)) {
    chunks$Author <- opts$creator
  }
  chunks$Description <- paste("Plot generated from R code:", plot_call)
  chunks$Software <- paste(
    "R", getRversion(), "using ggsaveR", utils::packageVersion("ggsaveR")
  )
  chunks[["Creation Time"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  if (!opts$embed_data) {
    return(chunks)
  }

  parts <- opts$embed_metadata
  chunks$ggsaveR_version <- as.character(utils::packageVersion("ggsaveR"))
  if ("call" %in% parts) {
    chunks$ggsaveR_call <- plot_call
  }
  if ("session_info" %in% parts) {
    chunks$ggsaveR_session <- paste(utils::capture.output(print(utils::sessionInfo())), collapse = "\n")
  }

  large <- list()
  if ("data" %in% parts) {
    csv <- data_as_csv(plot)
    if (!is.null(csv)) large$ggsaveR_data_csv <- encode_text(csv)
  }
  if ("plot" %in% parts) {
    large$ggsaveR_object <- encode_object(list(plot_object = plot))
  }

  size <- sum(nchar(unlist(large)))
  if (size > opts$embed_max_size) {
    warning(
      "The plot object and data for ", basename(filename), " would add ",
      format(structure(size, class = "object_size"), units = "auto"),
      " and were not embedded. Raise `ggsaveR.embed_max_size` to embed them.",
      call. = FALSE
    )
    large <- list()
  }
  c(chunks, large)
}

#' Write the plot data as CSV text, or return NULL if it cannot be done
#' @noRd
data_as_csv <- function(plot) {
  if (!inherits(plot, "ggplot") || !is.data.frame(plot$data)) {
    return(NULL)
  }
  tryCatch({
    con <- textConnection("lines", "w", local = TRUE)
    on.exit(close(con))
    utils::write.csv(as.data.frame(plot$data), con, row.names = FALSE)
    paste(lines, collapse = "\n")
  }, error = function(e) {
    warning("The plot data could not be written as CSV and were not embedded: ",
            conditionMessage(e), call. = FALSE)
    NULL
  })
}

#' Read reproducibility data embedded in a PNG
#'
#' Extracts the information that [ggsave()] embeds in a PNG file when the
#' `ggsaveR.embed_data` option is `TRUE`.
#'
#' @section Security:
#' The plot object is stored as a serialized R object. Restoring it with
#' [unserialize()] can run arbitrary code, so it is only read when you set
#' `trusted = TRUE`. Do that only for files that you, or someone you trust,
#' created. The call, session information and data are stored as plain text
#' and CSV, and are always safe to read.
#'
#' @section Reading the data outside R:
#' The data are stored in the PNG text chunk `ggsaveR_data_csv` as a
#' UTF-8 CSV file, compressed with zlib and base64 encoded. In Python, for
#' example: `zlib.decompress(base64.b64decode(text))`. The call and session
#' are in the plain-text chunks `ggsaveR_call` and `ggsaveR_session`, which
#' tools such as `exiftool` show directly.
#'
#' @param path Path to a PNG file written by [ggsave()].
#' @param trusted If `TRUE`, also restore the serialized plot object. Files
#'   written by ggsaveR 0.1.0, which stored everything as one serialized
#'   object, can only be read with `trusted = TRUE`.
#' @return A list with some or all of these elements, depending on the
#'   `ggsaveR.embed_metadata` option in effect when the file was saved:
#'   \describe{
#'     \item{`ggsaveR_version`}{Version of ggsaveR that wrote the file.}
#'     \item{`plot_call`}{The expression passed as `plot`, as a string.}
#'     \item{`session_info`}{The printed [sessionInfo()], as a string.}
#'     \item{`plot_data`}{The plot data as a data frame, read back from CSV.
#'       Column types are those [utils::read.csv()] infers; factor levels,
#'       dates and row names are not kept. Use `plot_object$data` for the
#'       exact data.}
#'     \item{`plot_object`}{The plot object, only with `trusted = TRUE`.}
#'   }
#'   Returns `NULL`, with a warning, if the file holds no ggsaveR data or
#'   cannot be read.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' out <- file.path(tempdir(), "embedded.png")
#'
#' old <- ggsaveR_options(embed_data = TRUE)
#' ggsave(out, p, width = 5, height = 4)
#' options(old)
#'
#' info <- read_ggsaveR_data(out)
#' info$plot_call
#' head(info$plot_data)
#'
#' # The plot object itself: only for files you trust
#' restored <- read_ggsaveR_data(out, trusted = TRUE)$plot_object
#' @export
read_ggsaveR_data <- function(path, trusted = FALSE) {
  if (!is_string(path)) {
    stop("`path` must be a single string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  if (tolower(tools::file_ext(path)) != "png") {
    stop("Data embedding is only supported for PNG files.", call. = FALSE)
  }
  if (!isTRUE(trusted) && !isFALSE(trusted)) {
    stop("`trusted` must be TRUE or FALSE.", call. = FALSE)
  }

  tryCatch(
    read_chunks(path, attr(png::readPNG(path, info = TRUE), "metadata", exact = TRUE), trusted),
    error = function(e) {
      warning(
        "Could not read ggsaveR data from '", path, "': ", conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

read_chunks <- function(path, meta, trusted) {
  # Files from ggsaveR 0.1.0 hold one serialized list in `ggsaveR_data`
  if (is.null(meta$ggsaveR_version) && !is.null(meta$ggsaveR_data)) {
    if (!trusted) {
      warning(
        "'", path, "' was written by an older version of ggsaveR that stores ",
        "everything as a serialized R object. Use `trusted = TRUE` to read it, ",
        "but only if you trust the file.",
        call. = FALSE
      )
      return(NULL)
    }
    return(decode_object(meta$ggsaveR_data))
  }

  if (is.null(meta$ggsaveR_version)) {
    warning("No ggsaveR data found in '", path, "'.", call. = FALSE)
    return(NULL)
  }

  out <- list(
    ggsaveR_version = package_version(meta$ggsaveR_version),
    plot_call = meta$ggsaveR_call,
    session_info = meta$ggsaveR_session
  )
  if (!is.null(meta$ggsaveR_data_csv)) {
    out$plot_data <- utils::read.csv(
      text = decode_text(meta$ggsaveR_data_csv),
      check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8"
    )
  }
  if (!is.null(meta$ggsaveR_object)) {
    if (trusted) {
      out$plot_object <- decode_object(meta$ggsaveR_object)$plot_object
    } else {
      message(
        "'", basename(path), "' also holds the plot object. ",
        "Use `trusted = TRUE` to restore it, but only if you trust the file."
      )
    }
  }
  Filter(Negate(is.null), out)
}

# Text -> UTF-8 -> zlib -> base64
encode_text <- function(x) {
  base64enc::base64encode(memCompress(charToRaw(enc2utf8(x)), type = "gzip"))
}

decode_text <- function(x) {
  out <- rawToChar(memDecompress(base64enc::base64decode(x), type = "gzip"))
  Encoding(out) <- "UTF-8"
  out
}

# R object -> serialize -> zlib -> base64 (the format used by ggsaveR 0.1.0)
encode_object <- function(x) {
  base64enc::base64encode(memCompress(serialize(x, NULL), type = "gzip"))
}

decode_object <- function(x) {
  unserialize(memDecompress(base64enc::base64decode(x), type = "gzip"))
}
