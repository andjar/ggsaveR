# read_data.R

#' @importFrom tools file_ext
#' @importFrom png readPNG
#' @importFrom base64enc base64decode

#' @title Read embedded reproducibility data from a ggsaveR-generated file
#'
#' @description This function extracts reproducibility data embedded in a file by
#' `ggsaveR::ggsave()`. It is optimized to read `tEXt` chunks from PNG
#' files natively.
#'
#' @param path The path to the file.
#' @return A list containing the embedded R objects (e.g., 'plot_call',
#' 'session_info'). Returns `NULL` if no `ggsaveR` data is found.
#' @export
read_ggsaveR_data <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  dev <- tolower(tools::file_ext(path))

  if (dev != "png") {
    warning(
      "This function is currently optimized for reading ggsaveR data from PNG files. ",
      "Support for other formats is not yet implemented in this version.", call. = FALSE
    )
    return(NULL)
  }

  tryCatch({
    # readPNG with info=TRUE gets metadata without reading the large image raster
    png_info <- png::readPNG(path, info = TRUE)

    # The metadata is in the 'text' attribute
    text_chunks <- attr(png_info, "text", exact = TRUE)

    if (is.null(text_chunks) || is.null(text_chunks$ggsaveR_data)) {
      warning("No ggsaveR_data chunk found in this PNG file.", call. = FALSE)
      return(NULL)
    }

    encoded_data <- text_chunks$ggsaveR_data

    # Reverse the pipeline: Base64 Decode -> Decompress -> Unserialize
    compressed_data <- base64enc::base64decode(encoded_data)
    serialized_data <- memDecompress(compressed_data, type = "gzip")
    unserialized_data <- unserialize(serialized_data)

    return(unserialized_data)

  }, error = function(e) {
    warning("Could not read metadata from '", path, "'. It may not be a valid PNG or may be corrupt. Error: ", e$message)
    return(NULL)
  })
}
