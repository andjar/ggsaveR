# ggsave.R

#' @importFrom rlang call_args `%||%`
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom ragg agg_png
#' @importFrom png writePNG
#' @importFrom base64enc base64encode
#' @importFrom utils packageVersion

# You will need to add ragg, png, and base64enc to your DESCRIPTION file:
# Imports:
#   ragg,
#   png,
#   base64enc

# Your unique_filename helper is perfect.
unique_filename <- function(path) {
  if (!file.exists(path)) return(path)
  base <- tools::file_path_sans_ext(path)
  ext <- tools::file_ext(path)
  i <- 1
  repeat {
    new_path <- sprintf("%s-%d.%s", base, i, ext)
    if (!file.exists(new_path)) return(new_path)
    i <- i + 1
  }
}

#' Saves a PNG with embedded R metadata using native R packages
#'
#' This function replaces the default ggsave behavior for PNGs. It renders
#' the plot using the {ragg} device, prepares metadata (standard and custom),
#' and writes the final PNG with embedded text chunks using the {png} package.
#' @noRd
save_png_with_data <- function(filename, plot, plot_call_str, creator, embed_data, ...) {

  # --- 1. Prepare metadata payload ---
  text_chunks <- list()

  # Add standard, human-readable metadata
  if (!is.null(creator)) {
    text_chunks$Author <- creator
  }
  text_chunks$Description <- paste("Plot generated from R code:", plot_call_str)
  text_chunks$Software <- paste("R", getRversion(), "using ggsaveR", packageVersion("ggsaveR"))
  text_chunks$CreationTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  # Add custom, machine-readable data blob for reproducibility
  if (isTRUE(embed_data)) {
    message("Embedding reproducibility data into '", filename, "'.")
    repro_data <- list(
      ggsaveR_version = packageVersion("ggsaveR"),
      plot_call = plot_call_str,
      session_info = sessionInfo()
    )

    # Pipeline: Serialize R object -> Compress -> Base64 Encode for tEXt chunk
    serialized_data <- serialize(repro_data, NULL)
    compressed_data <- memCompress(serialized_data, type = "gzip")
    encoded_data <- base64enc::base64encode(compressed_data)

    # Use a custom keyword. "Raw" is sometimes used, but a custom one is safer.
    text_chunks$ggsaveR_data <- encoded_data
  }

  # --- 2. Render the plot to a raster array in memory ---
  # We pass ggsave args like width, height, dpi, etc., to ragg::agg_png.
  ggsave_args <- list(...)
  render_args <- list(
    filename = NULL, # This is key: tells ragg to return the raster, not save to file
    width = ggsave_args$width %||% 7,
    height = ggsave_args$height %||% 7,
    units = ggsave_args$units %||% "in",
    dpi = ggsave_args$dpi %||% 300,
    background = ggsave_args$bg %||% "white",
    res = ggsave_args$dpi %||% 300 # ragg uses `res` for dpi
  )

  # Capture the plot rendered by ragg
  captured_plot <- tryCatch({
    ragg::agg_png(
      filename = render_args$filename,
      width = render_args$width,
      height = render_args$height,
      units = render_args$units,
      res = render_args$res,
      background = render_args$background
    )
    print(plot)
    invisible(dev.off())
  }, error = function(e) {
    # Ensure device is closed on error
    if (dev.cur() != 1) dev.off()
    stop("Failed to render plot in memory using {ragg}: ", e$message, call. = FALSE)
  })

  # --- 3. Write the final PNG with the plot image and metadata ---
  tryCatch({
    png::writePNG(image = captured_plot, target = filename, metadata = text_chunks)
    message("Successfully saved '", filename, "' with metadata.")
  }, error = function(e) {
    stop("Failed to write final PNG file '", filename, "': ", e$message, call. = FALSE)
  })
}


# --- Your main ggsave function ---
# This remains almost identical, but now calls the working helper.
# I've modified the `save_png_with_data` call to pass `embed_data`.
#' @export
ggsave <- function(filename, plot = last_plot(), device = NULL, ..., guard = FALSE) {

  if (isTRUE(guard)) {
    call <- match.call()
    call$guard <- NULL
    call[[1]] <- quote(ggplot2::ggsave)
    return(eval.parent(call))
  }

  plot_arg <- substitute(plot)
  plot_call_str <- paste(deparse(plot_arg), collapse = "\n")

  formats <- getOption("ggsaveR.formats", NULL)
  overwrite_action <- getOption("ggsaveR.overwrite_action", "overwrite")
  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  creator <- getOption("ggsaveR.creator", NULL)

  user_args <- list(...)
  saved_files <- character()
  base_filename <- tools::file_path_sans_ext(filename)

  # Function to handle a single save operation
  # This avoids code duplication between the two main branches
  process_single_save <- function(dev, current_filename, args) {

    # Handle data embedding for PNGs vs. other formats
    is_png <- tolower(dev) == "png"

    if (is_png && (embed_data || !is.null(creator))) {
      # Use our special native PNG handler
      do.call(
        save_png_with_data,
        c(list(filename = current_filename, plot = plot, plot_call_str = plot_call_str,
               creator = creator, embed_data = embed_data), args)
      )
    } else {
      # Fallback to standard ggsave for other formats (e.g., PDF, SVG)
      # or for PNGs when no metadata is being added.

      # For other formats, inject creator if possible.
      # ggplot2::ggsave passes 'author' to pdf(), but not other devices.
      if (!is.null(creator) && !("author" %in% names(args))) {
        args$author <- creator
      }

      do.call(ggplot2::ggsave, c(list(filename = current_filename, plot = plot, device = dev), args))
    }
  }

  if (is.list(formats) && !is.data.frame(formats)) {
    # --- A. NEW BEHAVIOR: Use ggsaveR.formats option ---
    for (fmt_config in formats) {
      dev <- fmt_config$device
      if (is.null(dev)) {
        warning("A format in `ggsaveR.formats` is missing a 'device'. Skipping.", call. = FALSE)
        next
      }
      current_filename <- paste0(base_filename, ".", dev)
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") stop("File '", current_filename, "' already exists.", call. = FALSE)
        if (overwrite_action == "unique") current_filename <- unique_filename(current_filename)
      }

      final_args <- c(user_args, fmt_config)
      process_single_save(dev, current_filename, final_args)
      saved_files <- c(saved_files, current_filename)
    }
  } else {
    # --- B. FALLBACK BEHAVIOR: Use `device` argument ---
    devices <- device %||% tolower(tools::file_ext(filename))
    if (length(devices) == 0 || all(devices == "")) {
      stop("Cannot determine device. Please specify `device`.", call. = FALSE)
    }

    for (dev in devices) {
      current_filename <- paste0(base_filename, ".", dev)
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") stop("File '", current_filename, "' already exists.", call. = FALSE)
        if (overwrite_action == "unique") current_filename <- unique_filename(current_filename)
      }

      process_single_save(dev, current_filename, user_args)
      saved_files <- c(saved_files, current_filename)
    }
  }

  invisible(saved_files)
}
