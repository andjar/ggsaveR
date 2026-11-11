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

  ext <- tools::file_ext(paths)
  stem <- ifelse(ext == "", paths, tools::file_path_sans_ext(paths))
  suffix <- ifelse(ext == "", "", paste0(".", ext))

  i <- 1L
  repeat {
    new_paths <- paste0(stem, "-", i, suffix)
    if (!any(file.exists(new_paths))) break
    i <- i + 1L
  }

  message(
    "File exists. Saving to ", paste(basename(new_paths), collapse = ", "),
    " instead."
  )
  new_paths
}
