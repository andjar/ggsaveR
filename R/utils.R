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



