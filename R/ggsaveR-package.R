#' ggsaveR: a drop-in `ggsave()` with extra features
#'
#' ggsaveR masks [ggplot2::ggsave()] with a version whose extra behaviour is
#' controlled entirely by global options. Load the package and set the options
#' at the top of a script, and every existing `ggsave()` call picks them up.
#'
#' @section Options:
#' Set the options with [ggsaveR_options()], which checks them, or with
#' [options()] using the names below.
#' \describe{
#'   \item{`ggsaveR.formats`}{`NULL` (default), a character vector of devices,
#'     or a list of lists that each hold a `device`, optional `ggsave()`
#'     arguments such as `width`, `height`, `units` and `dpi`, and an optional
#'     `suffix`. One file is written per format.}
#'   \item{`ggsaveR.size_suffix`}{`"auto"` (default), `TRUE` or `FALSE`. Add
#'     the size to file names, as in `plot_120x80mm_300dpi.png`; `"auto"` does
#'     so only when file names would otherwise clash.}
#'   \item{`ggsaveR.dir`}{`NULL` (default) or a directory for all plots with a
#'     relative file name. Created if needed.}
#'   \item{`ggsaveR.overwrite_action`}{`"overwrite"` (default), `"unique"` or
#'     `"stop"`.}
#'   \item{`ggsaveR.variants`}{`NULL` (default) or any of `"greyscale"`,
#'     `"deutan"`, `"protan"` and `"tritan"`. Also save these versions as PNG
#'     files; see [preview_variants()].}
#'   \item{`ggsaveR.embed_data`}{`FALSE` (default) or `TRUE`. Embed
#'     reproducibility data in PNG files; see [read_ggsaveR_data()].}
#'   \item{`ggsaveR.embed_metadata`}{Which parts to embed: any of `"plot"`,
#'     `"data"`, `"session_info"` and `"call"` (default: all).}
#'   \item{`ggsaveR.embed_max_size`}{Largest size in bytes of the embedded plot
#'     object and data (default 5 MB).}
#'   \item{`ggsaveR.creator`}{`NULL` (default) or a string stored as the
#'     `Author` text field of PNG files.}
#'   \item{`ggsaveR.log`}{`NULL` (default) or the path of a CSV file that
#'     records every saved file.}
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
