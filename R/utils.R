#' Generate a unique filename to avoid overwriting
#'
#' @param path The full path to the file.
#' @return A unique file path.
#' @noRd
unique_filename <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }

  dir <- dirname(path)
  filename <- tools::file_path_sans_ext(basename(path))
  ext <- tools::file_ext(path)

  i <- 1
  new_path <- file.path(dir, paste0(filename, "-", i, ".", ext))

  while (file.exists(new_path)) {
    i <- i + 1
    new_path <- file.path(dir, paste0(filename, "-", i, ".", ext))
  }

  message("File exists. Saving to ", basename(new_path), " instead.")
  return(new_path)
}


#' Save a ggplot to PNG with embedded data
#'
#' This function handles the special case of saving a PNG with embedded
#' reproducibility data.
#'
#' @inheritParams ggplot2::ggsave
#' @param filename The final filename for the PNG.
#' @param plot_call_str A string representing the call used to generate the plot.
#' @param creator A string for the author/creator metadata field.
#' @return Invisibly returns the filename.
#' @noRd
save_png_with_data <- function(filename, plot, plot_call_str = NULL, creator = NULL, ...) {

  # 1. Create the plot in memory (same as before)
  tmp <- tempfile(fileext = ".png")
  # Pass ... down to the internal ggsave call (for width, height, dpi, etc.)
  args <- list(filename = tmp, plot = plot, ...)
  do.call(ggplot2::ggsave, args)
  img_data <- png::readPNG(tmp, native = FALSE)
  unlink(tmp)

  # 2. Prepare the metadata for embedding
  metadata_to_embed <- list()

  # Get options for what to embed
  embed_opts <- getOption("ggsaveR.embed_metadata", c("plot", "data", "session_info", "call"))

  if ("plot" %in% embed_opts) {
    metadata_to_embed$plot_object <- plot
  }
  if ("data" %in% embed_opts && !is.null(plot$data)) {
    metadata_to_embed$plot_data <- plot$data
    message("Note: Raw data will be embedded in this png file. Make sure you understand the risk of this.")
  }
  if ("session_info" %in% embed_opts) {
    metadata_to_embed$session_info <- sessionInfo()
  }
  # Add the plot call to the metadata if requested
  if ("call" %in% embed_opts && !is.null(plot_call_str)) {
    metadata_to_embed$plot_call <- plot_call_str
  }

  # 3. Serialize and encode the metadata (same as before)
  if (length(metadata_to_embed) > 0) {
    serialized_data <- serialize(metadata_to_embed, NULL)
    encoded_data <- base64enc::base64encode(serialized_data)
    png_metadata <- list(ggsaveR_data = encoded_data)
    message("Embedded reproducibility data into ", basename(filename))
  }

  # --- Handle standard metadata fields ---
  if (!is.null(creator)) {
    png_metadata$Author <- creator
  }

  # 3. Write the final PNG with the embedded data
  if (length(png_metadata) > 0) {
    png::writePNG(img_data, target = filename, metadata = png_metadata)
  } else {
    # If for some reason this was called with nothing to do, just write the file
    png::writePNG(img_data, target = filename)
  }

  invisible(filename)
}
