.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ggsaveR <- list(
    # Default behavior is to overwrite, matching ggplot2::ggsave
    ggsaveR.overwrite_action = "overwrite",

    # Do not embed data by default
    ggsaveR.embed_data = FALSE,

    # Default metadata to embed
    ggsaveR.embed_metadata = c("plot", "data", "session_info", "call"),
    ggsaveR.creator = NULL
  )

  toset <- !(names(op.ggsaveR) %in% names(op))
  if (any(toset)) options(op.ggsaveR[toset])

  invisible()
}
