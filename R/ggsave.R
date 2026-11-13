#' Save a ggplot (or other grid object) with ggsaveR enhancements
#'
#' A drop-in replacement for [ggplot2::ggsave()]. Called without any `ggsaveR.*`
#' options set, it behaves like the original. Setting options switches on extra
#' behaviour for every `ggsave()` call in a script. See [ggsaveR_options()] for
#' the full list.
#'
#' @section Options:
#' * `ggsaveR.formats`: save every plot in several formats and sizes, either as
#'   a character vector of devices (`c("png", "pdf")`) or as a list of lists,
#'   each with a `device` and any other `ggsave()` arguments
#'   (`list(device = "png", width = 120, height = 80, units = "mm", dpi = 300)`).
#'   The extension of `filename` is replaced and the `device` argument is
#'   ignored. Settings in a format take precedence over arguments in the call.
#' * `ggsaveR.size_suffix`: add the size to file names, as in
#'   `plot_120x80mm_300dpi.png`. By default (`"auto"`) this happens only when
#'   two formats would otherwise write to the same file.
#' * `ggsaveR.dir`: save all plots with a relative file name into this
#'   directory.
#' * `ggsaveR.overwrite_action`: `"overwrite"` (default), `"unique"` or
#'   `"stop"` when a file already exists.
#' * `ggsaveR.variants`: also save greyscale and colour vision deficiency
#'   versions as PNG files, such as `plot_greyscale.png` and `plot_deutan.png`.
#' * `ggsaveR.embed_data`, `ggsaveR.embed_metadata`, `ggsaveR.embed_max_size`:
#'   embed the plot expression, data, session information and plot object in
#'   PNG files. Read them back with [read_ggsaveR_data()].
#' * `ggsaveR.creator`: a string stored as the `Author` of PNG files.
#' * `ggsaveR.log`: append a row describing every saved file to a CSV file.
#'
#' @section File names:
#' The final file name is built in this order: the directory from
#' `ggsaveR.dir` (for relative names), the stem of `filename`, a suffix
#' (an explicit `suffix` from the format, otherwise a size suffix if
#' `ggsaveR.size_suffix` calls for one), the extension of the device, and
#' finally `-1`, `-2`, ... if `ggsaveR.overwrite_action = "unique"` and the file
#' exists. Variants add `_greyscale` etc. to the name of the first format.
#'
#' @section Differences from ggplot2:
#' * `device` may be a character vector, in which case one file is written per
#'   device and the extension of `filename` is replaced.
#' * If `filename` has no extension and `device` is a single string, the
#'   device is appended as the extension.
#' * A character vector of saved paths is returned (invisibly), rather than
#'   the single `filename`.
#'
#' @section Embedded data and privacy:
#' With `ggsaveR.embed_data = TRUE` the PNG contains the **raw data** behind the
#' plot (all columns, not only the plotted ones) and, if `"plot"` is embedded,
#' the plot object, which for plots built inside a function includes that
#' function's environment. Do not share such files if the data are sensitive.
#' Use `ggsaveR.embed_metadata` to embed less, or `guard = TRUE` to skip a
#' single plot.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param device Device to use: `NULL` (guess from the extension of
#'   `filename`), a device name such as `"png"`, a character vector of device
#'   names, or a device function. See [ggplot2::ggsave()]. Ignored when the
#'   `ggsaveR.formats` option is set.
#' @param path Directory to save the plot in, combined with `filename` as in
#'   [ggplot2::ggsave()].
#' @param ... Other arguments passed on to [ggplot2::ggsave()], such as
#'   `width`, `height`, `units`, `dpi`, `scale` or `bg`.
#' @param guard If `TRUE`, all ggsaveR enhancements are bypassed and the call
#'   is forwarded unchanged to [ggplot2::ggsave()].
#' @return Invisibly, a character vector with the paths of the files created.
#' @seealso [ggsaveR_options()] for all options, [read_ggsaveR_data()] to read
#'   embedded data back, [preview_variants()] to check colours interactively.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
#' out <- file.path(tempdir(), "mtcars.png")
#'
#' # Works like ggplot2::ggsave()
#' ggsave(out, p, width = 5, height = 4)
#'
#' # Save every plot as PNG and PDF, and never overwrite
#' old <- ggsaveR_options(formats = c("png", "pdf"), overwrite_action = "unique")
#' ggsave(out, p, width = 5, height = 4)
#' options(old)
#'
#' # Two sizes of the same format get the size in their file names:
#' # mtcars_120x80mm_300dpi.png and mtcars_240x160mm_150dpi.png
#' old <- ggsaveR_options(formats = list(
#'   list(device = "png", width = 120, height = 80, units = "mm", dpi = 300),
#'   list(device = "png", width = 240, height = 160, units = "mm", dpi = 150)
#' ))
#' ggsave(out, p)
#' options(old)
#'
#' # Bypass all options for one call
#' ggsave(out, p, width = 5, height = 4, guard = TRUE)
#' @export
ggsave <- function(filename, plot = last_plot(), device = NULL, path = NULL, ...,
                   guard = FALSE) {

  if (isTRUE(guard)) {
    call <- match.call()
    call[[1L]] <- quote(ggplot2::ggsave)
    call$guard <- NULL
    return(eval.parent(call))
  }

  plot_call <- plot_call_string(substitute(plot))
  opts <- get_options()
  user_args <- list(...)

  if (!is_string(filename)) {
    stop("`filename` must be a single string.", call. = FALSE)
  }
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  if (!is.null(opts$dir) && !is_absolute_path(filename)) {
    filename <- file.path(opts$dir, filename)
  }

  targets <- resolve_targets(filename, device, opts$formats, user_args)
  targets <- apply_suffixes(targets, opts$size_suffix)

  # Variants are named after the first target and use its size
  first <- targets[[1L]]
  variant_files <- if (length(opts$variants) > 0L) {
    paste0(tools::file_path_sans_ext(first$filename), "_", opts$variants, ".png")
  } else {
    character()
  }

  paths <- resolve_overwrite(
    c(vapply(targets, function(t) t$filename, character(1)), variant_files),
    opts$overwrite_action
  )
  target_paths <- paths[seq_along(targets)]
  variant_paths <- paths[-seq_along(targets)]

  if (!is.null(opts$dir)) {
    for (d in unique(dirname(paths))) {
      if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    }
  }

  for (i in seq_along(targets)) {
    save_one(
      filename = target_paths[[i]],
      plot = plot,
      device = targets[[i]]$device,
      args = targets[[i]]$args,
      plot_call = plot_call,
      opts = opts
    )
  }

  if (length(variant_paths) > 0L) {
    save_variants(plot, first$args, variant_paths, opts$variants)
  }

  if (!is.null(opts$log)) {
    write_log(
      log = opts$log,
      paths = paths,
      devices = c(
        vapply(targets, function(t) device_name(t$device, t$filename), character(1)),
        rep("png", length(variant_paths))
      ),
      args = c(lapply(targets, function(t) t$args), rep(list(first$args), length(variant_paths))),
      plot_call = plot_call
    )
  }

  invisible(paths)
}

#' Work out which files a call should produce
#'
#' @return A list of targets, each a list with `filename`, `device`, `args`
#'   and optionally `suffix`.
#' @noRd
resolve_targets <- function(filename, device, formats, user_args) {
  base <- tools::file_path_sans_ext(filename)

  # A. The ggsaveR.formats option
  if (!is.null(formats)) {
    targets <- list()
    for (fmt in formats) {
      dev <- fmt$device
      if (is.null(dev)) {
        warning("A format in `ggsaveR.formats` is missing a 'device'. Skipping.",
                call. = FALSE)
        next
      }
      suffix <- fmt$suffix
      fmt$device <- NULL
      fmt$suffix <- NULL
      targets[[length(targets) + 1L]] <- list(
        filename = paste0(base, ".", dev),
        device = dev,
        # Format-specific settings win over arguments given in the call
        args = utils::modifyList(user_args, fmt),
        suffix = suffix
      )
    }
    if (length(targets) == 0L) {
      stop("`ggsaveR.formats` does not define any usable format.", call. = FALSE)
    }
    return(targets)
  }

  # B. Several devices given in the call
  if (is.character(device) && length(device) > 1L) {
    return(lapply(device, function(dev) {
      list(filename = paste0(base, ".", dev), device = dev, args = user_args)
    }))
  }

  # C. A single file, as in ggplot2::ggsave()
  if (tools::file_ext(filename) == "") {
    if (is.character(device)) {
      filename <- paste0(filename, ".", device)
    } else if (is.null(device)) {
      stop("Cannot determine device. Please specify `device`.", call. = FALSE)
    }
  }
  list(list(filename = filename, device = device, args = user_args))
}

#' Apply `ggsaveR.overwrite_action` to a set of paths
#' @noRd
resolve_overwrite <- function(paths, action) {
  exists <- file.exists(paths)
  if (!any(exists) || action == "overwrite") {
    return(paths)
  }
  if (action == "stop") {
    stop(
      sprintf(ngettext(sum(exists), "File %s already exists.", "Files %s already exist."),
              paste0("'", paths[exists], "'", collapse = ", ")),
      call. = FALSE
    )
  }
  unique_filename(paths)
}

#' Save one file, adding PNG metadata if requested
#' @noRd
save_one <- function(filename, plot, device, args, plot_call, opts) {
  is_png <- device_name(device, filename) == "png"
  add_metadata <- is_png && (opts$embed_data || !is.null(opts$creator))

  if (!add_metadata) {
    do.call(
      ggplot2::ggsave,
      c(list(filename = filename, plot = plot, device = device), args)
    )
    return(invisible(filename))
  }

  target_dir <- dirname(filename)
  if (!dir.exists(target_dir)) {
    if (isTRUE(args$create.dir)) {
      dir.create(target_dir, recursive = TRUE)
    } else {
      stop("Cannot find directory '", target_dir, "'.", call. = FALSE)
    }
  }

  # Let ggplot2 render the PNG exactly as it normally would, then rewrite it
  # with the extra text chunks. This keeps `scale`, `dpi`, `bg`, `limitsize`
  # and the choice of device identical to a plain ggsave() call.
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  do.call(
    ggplot2::ggsave,
    c(list(filename = tmp, plot = plot, device = device), args)
  )

  chunks <- png_text_chunks(plot, plot_call, opts, filename)
  img <- png::readPNG(tmp, info = TRUE)
  png::writePNG(
    img,
    target = filename,
    dpi = attr(img, "info")$dpi,
    metadata = chunks
  )

  if (opts$embed_data) {
    embedded <- sum(nchar(unlist(chunks[grepl("^ggsaveR_", names(chunks))])))
    message(
      "Embedded reproducibility data into ", basename(filename),
      " (", format(structure(embedded, class = "object_size"), units = "auto"), ")"
    )
  }
  invisible(filename)
}

#' The device name of a target, in lower case, falling back on the extension
#' @noRd
device_name <- function(device, filename) {
  tolower(if (is.character(device)) device else tools::file_ext(filename))
}

#' Describe the `plot` argument as code
#'
#' Only symbols and calls are deparsed. When `ggsave()` is called through
#' `do.call()` the argument is the plot object itself, and deparsing that would
#' produce thousands of lines.
#' @noRd
plot_call_string <- function(expr) {
  if (is.symbol(expr) || is.call(expr)) {
    paste(deparse(expr), collapse = "\n")
  } else {
    "<plot object>"
  }
}
