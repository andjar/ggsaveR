#' Read embedded data from a ggsaveR-generated PNG file
#'
#' This function extracts the reproducibility data (e.g., ggplot object,
#' data frame) that was embedded in a PNG file by `ggsaveR::ggsave()`.
#'
#' @param path The path to the PNG file.
#' @return A list containing the embedded R objects (e.g., 'plot_object',
#'   'plot_data', 'session_info'). Returns `NULL` if no ggsaveR data is found.
#' @export
read_ggsaveR_data <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  if (tolower(tools::file_ext(path)) != "png") {
    stop("Data embedding is only supported for PNG files.", call. = FALSE)
  }

  # Read the PNG. The metadata is automatically attached as an attribute
  # by the png package, so we don't need a special argument.
  img <- png::readPNG(path, native = FALSE)

  # Retrieve the metadata from the object's attributes
  metadata <- attr(img, "metadata")

  if (is.null(metadata) || is.null(metadata$ggsaveR_data)) {
    warning("No ggsaveR metadata found in this PNG file.", call. = FALSE)
    return(NULL)
  }

  # Decode and unserialize the data
  encoded_data <- metadata$ggsaveR_data
  serialized_data <- base64enc::base64decode(encoded_data)
  unserialized_data <- unserialize(serialized_data)

  return(unserialized_data)
}
