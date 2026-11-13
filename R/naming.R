raster_devices <- c("png", "jpeg", "jpg", "tiff", "tif", "bmp")

#' Add explicit and size-based suffixes to the targets' file names
#'
#' With `size_suffix = "auto"`, only targets whose file name clashes with
#' another target get the size suffix, so that e.g. two PNG formats of
#' different sizes no longer overwrite each other.
#' @noRd
apply_suffixes <- function(targets, size_suffix) {
  files <- vapply(targets, function(t) t$filename, character(1))
  clashing <- files %in% files[duplicated(files)]

  for (i in seq_along(targets)) {
    t <- targets[[i]]
    suffix <- t$suffix
    if (is.null(suffix) && (isTRUE(size_suffix) || (identical(size_suffix, "auto") && clashing[i]))) {
      suffix <- size_label(t$device, t$filename, t$args)
    }
    if (!is.null(suffix)) {
      targets[[i]]$filename <- insert_suffix(t$filename, suffix)
    }
  }

  files <- vapply(targets, function(t) t$filename, character(1))
  if (anyDuplicated(files)) {
    stop(
      "Several formats would write to ",
      paste0("'", unique(files[duplicated(files)]), "'", collapse = ", "),
      ". Give them different sizes, or a `suffix` in `ggsaveR.formats`.",
      call. = FALSE
    )
  }
  targets
}

#' Insert a suffix before the file extension
#' @noRd
insert_suffix <- function(path, suffix) {
  ext <- tools::file_ext(path)
  if (ext == "") {
    paste0(path, suffix)
  } else {
    paste0(tools::file_path_sans_ext(path), suffix, ".", ext)
  }
}

#' Describe the size of an output file, e.g. "_120x80mm_300dpi"
#'
#' The resolution is only included for raster formats, and not when the size
#' is given in pixels.
#' @noRd
size_label <- function(device, filename, args) {
  size <- output_size(args)
  label <- paste0("_", format_number(size$width), "x", format_number(size$height), size$units)
  if (is_raster(device, filename) && size$units != "px") {
    label <- paste0(label, "_", format_number(size$dpi), "dpi")
  }
  label
}

#' The size ggplot2::ggsave() will use for a given set of arguments
#'
#' Mirrors ggplot2's own rules: `scale` multiplies the given dimensions, and a
#' missing width or height is taken from the open graphics device, or is 7
#' inches if there is none.
#' @noRd
output_size <- function(args) {
  units <- args$units %||% "in"
  if (!is_string(units) || !units %in% c("in", "cm", "mm", "px")) {
    stop('`units` must be one of "in", "cm", "mm" or "px".', call. = FALSE)
  }
  dpi <- parse_dpi(args$dpi %||% 300)
  scale <- args$scale %||% 1
  per_inch <- c(`in` = 1, cm = 2.54, mm = 25.4, px = dpi)[[units]]

  inches <- c(args$width %||% NA_real_, args$height %||% NA_real_) / per_inch * scale
  if (anyNA(inches)) {
    default <- if (length(grDevices::dev.list()) == 0L) c(7, 7) else grDevices::dev.size() * scale
    inches[is.na(inches)] <- default[is.na(inches)]
  }
  dims <- inches * per_inch
  list(width = dims[[1]], height = dims[[2]], units = units, dpi = dpi)
}

parse_dpi <- function(dpi) {
  if (is.character(dpi)) {
    dpi <- switch(dpi, screen = 72, print = 300, retina = 320,
      stop('`dpi` must be a number or one of "screen", "print" or "retina".', call. = FALSE)
    )
  }
  dpi
}

format_number <- function(x) format(round(x, 2), trim = TRUE, scientific = FALSE, drop0trailing = TRUE)

is_raster <- function(device, filename) {
  type <- if (is.character(device)) device else tools::file_ext(filename)
  tolower(type) %in% raster_devices
}

is_absolute_path <- function(path) grepl("^(/|~|\\\\|[A-Za-z]:)", path)

#' Find file names that do not exist yet
#'
#' Appends `-1`, `-2`, ... to the file stem. All `paths` get the same suffix,
#' so the files written by one multi-format `ggsave()` call stay together.
#'
#' @param paths Character vector of file paths.
#' @return A character vector of paths, none of which exist.
#' @noRd
unique_filename <- function(paths) {
  if (!any(file.exists(paths))) {
    return(paths)
  }

  i <- 1L
  repeat {
    new_paths <- vapply(paths, insert_suffix, character(1), suffix = paste0("-", i), USE.NAMES = FALSE)
    if (!any(file.exists(new_paths))) break
    i <- i + 1L
  }

  message(
    "File exists. Saving to ", paste(basename(new_paths), collapse = ", "),
    " instead."
  )
  new_paths
}
