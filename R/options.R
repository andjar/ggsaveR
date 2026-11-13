embed_parts <- c("plot", "data", "session_info", "call")
overwrite_actions <- c("overwrite", "unique", "stop")
variant_types <- c("greyscale", "grayscale", "deutan", "protan", "tritan")
option_names <- c(
  "formats", "size_suffix", "dir", "overwrite_action", "variants",
  "embed_data", "embed_metadata", "embed_max_size", "creator", "log"
)

.onLoad <- function(libname, pkgname) {
  defaults <- list(
    # Match ggplot2::ggsave() unless the user asks for something else
    ggsaveR.overwrite_action = "overwrite",
    ggsaveR.size_suffix = "auto",
    ggsaveR.embed_data = FALSE,
    ggsaveR.embed_metadata = embed_parts,
    ggsaveR.embed_max_size = 5e6
  )
  toset <- !(names(defaults) %in% names(options()))
  if (any(toset)) options(defaults[toset])
  invisible()
}

#' Set or inspect ggsaveR options
#'
#' A convenience wrapper around [options()] for the `ggsaveR.*` options. The
#' new values are checked straight away, so a typo fails here rather than at
#' the first `ggsave()` call. Setting an option to `NULL` restores its
#' default.
#'
#' @param formats Save every plot in several formats and/or sizes. `NULL`
#'   (default), a character vector of devices such as `c("png", "pdf")`, or a
#'   list of lists, each with a `device` and any other [ggsave()] arguments,
#'   for example `list(device = "png", width = 120, height = 80, units = "mm",
#'   dpi = 300)`. A format may also hold a `suffix`, such as `"_slide"`, that
#'   is added to the file name.
#' @param size_suffix Whether to add the size to file names, as in
#'   `plot_120x80mm_300dpi.png`. `"auto"` (default) adds it only where two
#'   formats would otherwise write to the same file; `TRUE` always adds it;
#'   `FALSE` never does.
#' @param dir A directory for all plots. Relative file names are saved inside
#'   it, and it is created if needed. `NULL` (default) uses file names as
#'   given.
#' @param overwrite_action What to do when a file already exists:
#'   `"overwrite"` (default), `"unique"` (add `-1`, `-2`, ... to the name) or
#'   `"stop"` (error before anything is written).
#' @param variants Also save PNG copies that show how the plot looks without
#'   colour or to readers with a colour vision deficiency. Any of
#'   `"greyscale"` (or `"grayscale"`), `"deutan"`, `"protan"` and `"tritan"`.
#'   The colour vision simulations need the colorspace package. See
#'   [preview_variants()].
#' @param embed_data If `TRUE`, embed reproducibility information in PNG
#'   files. See [read_ggsaveR_data()].
#' @param embed_metadata Which parts to embed: any of `"plot"`, `"data"`,
#'   `"session_info"` and `"call"` (default: all).
#' @param embed_max_size Largest size, in bytes, of the embedded plot object
#'   and data in one file (default 5 MB). Above it they are left out with a
#'   warning, while the call and session information are still embedded. Use
#'   `Inf` for no limit.
#' @param creator A string stored as the `Author` of PNG files.
#' @param log Path to a CSV file. Every saved file is appended to it as a row
#'   with time, path, size, resolution, MD5 checksum and the plot expression.
#' @return With arguments, the previous values of the options that were
#'   changed, invisibly, in a form that can be passed to [options()]. Without
#'   arguments, a named list of the current values of all ggsaveR options.
#' @examples
#' old <- ggsaveR_options(formats = c("png", "pdf"), overwrite_action = "unique")
#' ggsaveR_options()
#' options(old)
#' @export
ggsaveR_options <- function(formats, size_suffix, dir, overwrite_action, variants,
                            embed_data, embed_metadata, embed_max_size, creator, log) {
  given <- as.list(match.call())[-1L]
  if (length(given) == 0L) {
    return(lapply(stats::setNames(nm = paste0("ggsaveR.", option_names)), getOption))
  }
  # Evaluate the supplied arguments in the caller, keeping explicit NULLs
  values <- mget(names(given), envir = environment())
  new <- stats::setNames(values, paste0("ggsaveR.", names(given)))

  old <- options(new)
  tryCatch(get_options(), error = function(e) {
    options(old)
    stop(conditionMessage(e), call. = FALSE)
  })
  invisible(old)
}

#' Read and validate the ggsaveR options
#'
#' Fails early with a clear message, rather than silently ignoring a typo such
#' as `ggsaveR.overwrite_action = "uniqe"`.
#' @noRd
get_options <- function() {
  action <- getOption("ggsaveR.overwrite_action", "overwrite")
  if (!is_string(action) || !action %in% overwrite_actions) {
    stop_option("overwrite_action", "must be one of ", quote_list(overwrite_actions))
  }

  size_suffix <- getOption("ggsaveR.size_suffix", "auto")
  if (!(isTRUE(size_suffix) || isFALSE(size_suffix) || identical(size_suffix, "auto"))) {
    stop_option("size_suffix", 'must be TRUE, FALSE or "auto"')
  }

  dir <- getOption("ggsaveR.dir", NULL)
  if (!is.null(dir) && !is_string(dir)) {
    stop_option("dir", "must be a single string or NULL")
  }

  variants <- getOption("ggsaveR.variants", NULL)
  if (length(variants) > 0L) {
    variants <- check_variants(variants, arg = "Option `ggsaveR.variants`")
  } else {
    variants <- character()
  }

  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  if (!isTRUE(embed_data) && !isFALSE(embed_data)) {
    stop_option("embed_data", "must be TRUE or FALSE")
  }

  embed_metadata <- getOption("ggsaveR.embed_metadata", embed_parts)
  if (length(setdiff(embed_metadata, embed_parts)) > 0L) {
    stop_option("embed_metadata", "can only contain ", quote_list(embed_parts))
  }

  embed_max_size <- getOption("ggsaveR.embed_max_size", 5e6)
  if (!is.numeric(embed_max_size) || length(embed_max_size) != 1L ||
      is.na(embed_max_size) || embed_max_size < 0) {
    stop_option("embed_max_size", "must be a single non-negative number")
  }

  creator <- getOption("ggsaveR.creator", NULL)
  if (!is.null(creator) && !is_string(creator)) {
    stop_option("creator", "must be a single string or NULL")
  }

  log <- getOption("ggsaveR.log", NULL)
  if (!is.null(log) && !is_string(log)) {
    stop_option("log", "must be a single string or NULL")
  }

  list(
    formats = normalise_formats(getOption("ggsaveR.formats", NULL)),
    size_suffix = size_suffix,
    dir = dir,
    overwrite_action = action,
    variants = variants,
    embed_data = embed_data,
    embed_metadata = embed_metadata,
    embed_max_size = embed_max_size,
    creator = creator,
    log = log
  )
}

#' Turn `ggsaveR.formats` into a list of lists
#' @noRd
normalise_formats <- function(formats) {
  if (is.null(formats)) {
    return(NULL)
  }
  if (is.character(formats)) {
    return(lapply(formats, function(dev) list(device = dev)))
  }
  ok <- is.list(formats) && !is.data.frame(formats) &&
    all(vapply(formats, is.list, logical(1)))
  if (!ok) {
    stop_option(
      "formats", "must be a character vector of devices or a list of lists, ",
      'e.g. `list(list(device = "png", dpi = 300))`'
    )
  }
  for (fmt in formats) {
    if (!is.null(fmt$device) && !is_string(fmt$device)) {
      stop_option("formats", "devices must be given as strings, such as \"png\"")
    }
    if (!is.null(fmt$suffix) && !is_string(fmt$suffix)) {
      stop_option("formats", "a `suffix` must be a single string")
    }
  }
  formats
}

#' Check and normalise a vector of variant names
#' @noRd
check_variants <- function(variants, arg = "`variants`") {
  if (!is.character(variants) || anyNA(variants) ||
      length(setdiff(variants, variant_types)) > 0L) {
    stop(arg, " can only contain ", quote_list(variant_types[-2L]), ".", call. = FALSE)
  }
  variants <- unique(variants)
  if (any(variants %in% c("deutan", "protan", "tritan")) &&
      !requireNamespace("colorspace", quietly = TRUE)) {
    stop(
      "Simulating colour vision deficiencies needs the colorspace package. ",
      'Install it with install.packages("colorspace").',
      call. = FALSE
    )
  }
  variants
}

stop_option <- function(name, ...) {
  stop("Option `ggsaveR.", name, "` ", ..., ".", call. = FALSE)
}

quote_list <- function(x) paste0('"', x, '"', collapse = ", ")

is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

`%||%` <- function(x, y) if (is.null(x)) y else x
