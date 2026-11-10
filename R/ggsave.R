#' Save a ggplot (or other grid object) with ggsaveR enhancements
#'
#' A drop-in replacement for [ggplot2::ggsave()]. Called without any `ggsaveR.*`
#' options set, it behaves like the original. Setting options switches on extra
#' behaviour for every `ggsave()` call in a script: saving to several formats at
#' once, protecting existing files, and embedding metadata in PNG files.
#'
#' @section Options:
#' All enhancements are controlled with [options()]; see [ggsaveR-package] for
#' the full reference.
#'
#' * `ggsaveR.formats`: save every plot in several formats and sizes. Either a
#'   character vector of devices (`c("png", "pdf")`) or a list of lists, each
#'   with a `device` and any other `ggsave()` arguments
#'   (`list(device = "png", width = 8, height = 5, dpi = 300)`). The extension
#'   of `filename` is replaced, and the `device` argument is ignored. Settings
#'   in a format take precedence over arguments given in the call.
#' * `ggsaveR.overwrite_action`: what to do when a file already exists:
#'   `"overwrite"` (default), `"stop"` (error, before anything is written) or
#'   `"unique"` (save as `plot-1.png`, `plot-2.png`, ...; all formats of one
#'   call share the same suffix).
#' * `ggsaveR.embed_data`: if `TRUE`, embed the plot object, its data, the
#'   call and the session info in PNG files. Read them back with
#'   [read_ggsaveR_data()]. `ggsaveR.embed_metadata` selects which parts.
#' * `ggsaveR.creator`: a string stored as the `Author` of PNG files.
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
#' plot (all columns, not only the plotted ones) and, for plots built inside a
#' function, every object in that function's environment. Do not share such
#' files if the data are sensitive, and expect larger files. Use
#' `ggsaveR.embed_metadata` to embed less, or `guard = TRUE` to skip a single
#' plot.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param device Device to use: `NULL` (guess from the extension of
#'   `filename`), a device name such as `"png"`, a character vector of device
#'   names, or a device function. See [ggplot2::ggsave()]. Ignored when the
#'   `ggsaveR.formats` option is set.
#' @param ... Other arguments passed on to [ggplot2::ggsave()], such as
#'   `width`, `height`, `units`, `dpi`, `scale` or `bg`.
#' @param guard If `TRUE`, all ggsaveR enhancements are bypassed and the call
#'   is forwarded unchanged to [ggplot2::ggsave()].
#' @return Invisibly, a character vector with the paths of the files created.
#' @seealso [read_ggsaveR_data()] to read embedded data back,
#'   [ggsaveR-package] for the options.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' out <- file.path(tempdir(), "mtcars.png")
#'
#' # Works like ggplot2::ggsave()
#' ggsave(out, p, width = 5, height = 4)
#'
#' # Save every plot as both PNG and PDF, and never overwrite
#' old <- options(
#'   ggsaveR.formats = c("png", "pdf"),
#'   ggsaveR.overwrite_action = "unique"
#' )
#' ggsave(out, p, width = 5, height = 4)
#'
#' # Bypass the options for one call
#' ggsave(out, p, width = 5, height = 4, guard = TRUE)
#'
#' options(old)
#' @export
ggsave <- function(filename, plot = last_plot(), device = NULL, ..., guard = FALSE) {

  if (isTRUE(guard)) {
    call <- match.call()
    call[[1L]] <- quote(ggplot2::ggsave)
    call$guard <- NULL
    return(eval.parent(call))
  }

  plot_call <- plot_call_string(substitute(plot))
  opts <- ggsaveR_options()
  user_args <- list(...)

  targets <- resolve_targets(filename, device, opts$formats, user_args)
  paths <- resolve_overwrite(
    vapply(targets, function(t) t$filename, character(1)),
    opts$overwrite_action
  )

  for (i in seq_along(targets)) {
    save_one(
      filename = paths[[i]],
      plot = plot,
      device = targets[[i]]$device,
      args = targets[[i]]$args,
      plot_call = plot_call,
      opts = opts
    )
  }

  invisible(paths)
}

#' Work out which files a call should produce
#'
#' @return A list of targets, each a list with `filename`, `device` and `args`.
#' @noRd
resolve_targets <- function(filename, device, formats, user_args) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("`filename` must be a single string.", call. = FALSE)
  }
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
      fmt$device <- NULL
      targets[[length(targets) + 1L]] <- list(
        filename = paste0(base, ".", dev),
        device = dev,
        # Format-specific settings win over arguments given in the call
        args = utils::modifyList(user_args, fmt)
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
  is_png <- if (is.character(device)) {
    tolower(device) == "png"
  } else {
    tolower(tools::file_ext(filename)) == "png"
  }
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

  chunks <- png_text_chunks(plot, plot_call, opts)
  img <- png::readPNG(tmp, info = TRUE)
  png::writePNG(
    img,
    target = filename,
    dpi = attr(img, "info")$dpi,
    metadata = chunks
  )

  if (!is.null(chunks$ggsaveR_data)) {
    message(
      "Embedded reproducibility data into ", basename(filename),
      " (", format(structure(nchar(chunks$ggsaveR_data), class = "object_size"),
                   units = "auto"), ")"
    )
  }
  invisible(filename)
}

#' Build the tEXt chunks written to a PNG
#' @noRd
png_text_chunks <- function(plot, plot_call, opts) {
  chunks <- list()
  if (!is.null(opts$creator)) {
    chunks$Author <- opts$creator
  }
  chunks$Description <- paste("Plot generated from R code:", plot_call)
  chunks$Software <- paste(
    "R", getRversion(), "using ggsaveR", utils::packageVersion("ggsaveR")
  )
  chunks[["Creation Time"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  if (opts$embed_data) {
    payload <- list()
    parts <- opts$embed_metadata
    if ("plot" %in% parts) {
      payload$plot_object <- plot
    }
    if ("data" %in% parts && inherits(plot, "ggplot") && !is.null(plot$data)) {
      payload$plot_data <- plot$data
    }
    if ("session_info" %in% parts) {
      payload$session_info <- utils::sessionInfo()
    }
    if ("call" %in% parts) {
      payload$plot_call <- plot_call
    }

    if (length(payload) > 0L) {
      payload <- c(list(ggsaveR_version = utils::packageVersion("ggsaveR")), payload)
      chunks$ggsaveR_data <- encode_payload(payload)
    }
  }
  chunks
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
