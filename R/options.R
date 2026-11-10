embed_parts <- c("plot", "data", "session_info", "call")
overwrite_actions <- c("overwrite", "unique", "stop")

.onLoad <- function(libname, pkgname) {
  defaults <- list(
    # Match ggplot2::ggsave() unless the user asks for something else
    ggsaveR.overwrite_action = "overwrite",
    ggsaveR.embed_data = FALSE,
    ggsaveR.embed_metadata = embed_parts
  )
  toset <- !(names(defaults) %in% names(options()))
  if (any(toset)) options(defaults[toset])
  invisible()
}

#' Read and validate the ggsaveR options
#'
#' Fails early with a clear message, rather than silently ignoring a typo such
#' as `ggsaveR.overwrite_action = "uniqe"`.
#' @noRd
ggsaveR_options <- function() {
  action <- getOption("ggsaveR.overwrite_action", "overwrite")
  if (!is.character(action) || length(action) != 1L || !action %in% overwrite_actions) {
    stop(
      "Option `ggsaveR.overwrite_action` must be one of ",
      paste0('"', overwrite_actions, '"', collapse = ", "), ".",
      call. = FALSE
    )
  }

  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  if (!isTRUE(embed_data) && !isFALSE(embed_data)) {
    stop("Option `ggsaveR.embed_data` must be TRUE or FALSE.", call. = FALSE)
  }

  embed_metadata <- getOption("ggsaveR.embed_metadata", embed_parts)
  unknown <- setdiff(embed_metadata, embed_parts)
  if (length(unknown) > 0L) {
    stop(
      "Option `ggsaveR.embed_metadata` can only contain ",
      paste0('"', embed_parts, '"', collapse = ", "), ".",
      call. = FALSE
    )
  }

  creator <- getOption("ggsaveR.creator", NULL)
  if (!is.null(creator) && !(is.character(creator) && length(creator) == 1L && !is.na(creator))) {
    stop("Option `ggsaveR.creator` must be a single string or NULL.", call. = FALSE)
  }

  list(
    formats = normalise_formats(getOption("ggsaveR.formats", NULL)),
    overwrite_action = action,
    embed_data = embed_data,
    embed_metadata = embed_metadata,
    creator = creator
  )
}

#' Turn `ggsaveR.formats` into a list of lists
#' @noRd
normalise_formats <- function(formats) {
  if (is.null(formats)) {
    return(NULL)
  }
  if (is.character(formats)) {
    return(lapply(formats, function(dev) list(device = dev)))
  }
  ok <- is.list(formats) && !is.data.frame(formats) &&
    all(vapply(formats, is.list, logical(1)))
  if (!ok) {
    stop(
      "Option `ggsaveR.formats` must be a character vector of devices or a ",
      "list of lists, e.g. `list(list(device = \"png\", dpi = 300))`.",
      call. = FALSE
    )
  }
  formats
}
