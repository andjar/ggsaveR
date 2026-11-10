#' ggsaveR: a drop-in `ggsave()` with extra features
#'
#' ggsaveR masks [ggplot2::ggsave()] with a version whose extra behaviour is
#' controlled entirely by global options. Load the package and set the options
#' at the top of a script, and every existing `ggsave()` call picks them up.
#'
#' @section Options:
#' \describe{
#'   \item{`ggsaveR.formats`}{`NULL` (default), a character vector of devices,
#'     or a list of lists that each hold a `device` and optional `ggsave()`
#'     arguments such as `width`, `height`, `units` and `dpi`. One file is
#'     written per format.}
#'   \item{`ggsaveR.overwrite_action`}{`"overwrite"` (default), `"unique"` or
#'     `"stop"`.}
#'   \item{`ggsaveR.embed_data`}{`FALSE` (default) or `TRUE`. Embed
#'     reproducibility data in PNG files; see [read_ggsaveR_data()].}
#'   \item{`ggsaveR.embed_metadata`}{Which parts to embed: any of `"plot"`,
#'     `"data"`, `"session_info"` and `"call"` (default: all).}
#'   \item{`ggsaveR.creator`}{`NULL` (default) or a string stored as the
#'     `Author` text field of PNG files.}
#' }
#'
#' Metadata (`ggsaveR.embed_data` and `ggsaveR.creator`) is only written to PNG
#' files; other formats are saved unchanged.
#'
#' @section Masking:
#' ggsaveR attaches ggplot2 when it is loaded, so `ggsaveR::ggsave()` masks
#' `ggplot2::ggsave()` regardless of the order of the `library()` calls. Use
#' `ggplot2::ggsave()` or `ggsave(..., guard = TRUE)` to reach the original.
#'
#' @importFrom ggplot2 last_plot
#' @keywords internal
"_PACKAGE"
