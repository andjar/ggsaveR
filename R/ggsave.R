#' @importFrom rlang call_args
#' @importFrom ggplot2 ggsave last_plot

#' Save a ggplot with extra features
#'
#' This function wraps [ggplot2::ggsave()] to provide additional features
#' such as saving to multiple formats at once, preventing file overwrites,
#' and embedding reproducibility data into PNG files.
#'
#' The new features are controlled via [options()]:
#' * `ggsaveR.overwrite_action`: What to do if the file exists. Can be
#'   `"overwrite"` (the default), `"unique"` (create a unique filename like
#'   `plot-1.png`), or `"stop"` (throw an error).
#' * `ggsaveR.embed_data`: A logical value. If `TRUE`, reproducibility data
#'   (the ggplot object, its data, and session info) will be embedded in the
#'   PNG file. This only applies to the PNG format.
#' * `ggsaveR.creator`: A character string. If set (e.g., `"Your Name"`), this
#'   will be embedded as the author/creator in the metadata of supported file
#'   types (like PNG and PDF).
#'
#' @param filename File name to create on disk. Can have multiple extensions
#'   if the `device` argument is a vector (e.g., `filename = "myplot"` with
#'   `device = c("png", "pdf")` will create `myplot.png` and `myplot.pdf`).
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param device Device to use. Can be a character vector of multiple devices,
#'   e.g., `c("png", "pdf", "svg")`.
#' @param ... Other arguments passed on to [ggplot2::ggsave()].
#'
#' @return Invisibly returns a vector of the saved file paths.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggsaveR)
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # --- Example 1: Save to multiple formats ---
#' # Creates plot.png and plot.pdf in a temporary directory
#' f <- tempfile(pattern = "plot")
#' ggsave(filename = f, plot = p, device = c("png", "pdf"))
#'
#' # --- Example 2: Avoid overwriting ---
#' options(ggsaveR.overwrite_action = "unique")
#' f_png <- paste0(f, ".png")
#' ggsave(f_png, p) # Saves plotXXXX.png
#' ggsave(f_png, p) # Saves plotXXXX-1.png
#'
#' # --- Example 3: Embed data for reproducibility ---
#' options(ggsaveR.embed_data = TRUE)
#' f_png_data <- tempfile(pattern = "plot_with_data", fileext = ".png")
#' ggsave(f_png_data, p)
#'
#' # Retrieve the embedded data
#' embedded_info <- read_ggsaveR_data(f_png_data)
#' print(names(embedded_info))
#' # You can now replot the original object
#' # embedded_info$plot_object
#'
#' # --- Reset options to default ---
#' options(
#'   ggsaveR.overwrite_action = "overwrite",
#'   ggsaveR.embed_data = FALSE
#' )
#'
#' # --- Example 4: Embed the plot call ---
#' options(ggsaveR.embed_data = TRUE)
#' f_png_call <- tempfile(pattern = "plot_with_call", fileext = ".png")
#'
#' # The call is captured when piping or nesting
#' ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#'   geom_point(aes(color = Species)) |>
#'   ggsave(filename = f_png_call)
#'
#' embedded_info <- read_ggsaveR_data(f_png_call)
#' # See the captured code
#' cat(embedded_info$plot_call)
#'
#' # --- Example 5: Add creator metadata ---
#' options(ggsaveR.creator = "Dr. G. G. Plotter")
#' f_pdf <- tempfile(pattern = "plot", fileext = ".pdf")
#' ggsave(f_pdf, p)
#' # Now open the PDF file and check its properties/metadata.
#' # You should see "Dr. G. G. Plotter" as the author.
#'
#' # Reset options
#' options(ggsaveR.creator = NULL)
#' }
ggsave <- function(filename, plot = last_plot(), device = NULL, ...) {

  # --- Capture the plot argument expression ---
  # We deparse the expression passed to the `plot` argument.
  # This is the magic that captures the code.
  plot_arg_expr <- rlang::call_args(match.call())$plot
  # deparse can return multiple lines, so we collapse them.
  plot_call_str <- paste(deparse(plot_arg_expr), collapse = "\n")

  # --- 1. Get options ---
  overwrite_action <- getOption("ggsaveR.overwrite_action", "overwrite")
  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  creator <- getOption("ggsaveR.creator", NULL)

  # --- Prepare arguments for the underlying save calls ---
  # Capture the ... args to modify them
  ggsave_args <- list(...)

  # Inject creator/author if it's set in options AND not already specified by the user
  # The 'author' argument is supported by pdf(), postscript(), and cairo_* devices.
  if (!is.null(creator) && !("author" %in% names(ggsave_args))) {
    ggsave_args$author <- creator
  }

  # --- 2. Handle multiple devices/dimensions ---
  # Base filename without extension
  base_filename <- tools::file_path_sans_ext(filename)

  # Check if device is a list for multiple dimensions
  if (is.list(device) && !is.data.frame(device)) {
    # Nested list for multiple formats and dimensions
    saved_files <- character(length(device))
    for (i in seq_along(device)) {
      format_opts <- device[[i]]
      dev <- format_opts$filetype
      width <- format_opts$width
      height <- format_opts$height
      units <- format_opts$units %||% "in" # Default units to inches if not specified

      # Construct filename with dimensions for uniqueness if needed
      current_filename <- paste0(base_filename, "_", width, "x", height, ".", dev)

      # Update ggsave_args with specific dimensions
      ggsave_args$width <- width
      ggsave_args$height <- height
      ggsave_args$units <- units

      # Handle file overwriting for this specific file
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") {
          stop("File '", current_filename, "' already exists.", call. = FALSE)
        } else if (overwrite_action == "unique") {
          current_filename <- unique_filename(current_filename)
        }
      }

      # Use the original ggsave for all other cases
       ggsave_args_final <- c(list(filename = current_filename, plot = plot, device = dev), ggsave_args)
      # Special handling for certain devices if necessary
      if (dev %in% c("jpg", "jpeg")) {
        ggsave_args_final$device <- "jpeg"
      } else if (dev == "tiff") {
        ggsave_args_final$device <- "tiff"
      } else if (dev == "eps") {
        ggsave_args_final$device <- "eps"
      }
      do.call(ggplot2::ggsave, ggsave_args_final)

      saved_files[i] <- current_filename
    }
    invisible(saved_files)
    return(invisible(saved_files))
  }

  # Fallback to original logic for simple vector of devices
  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
    if (device == "") {
      stop("Cannot determine device from filename with no extension.", call. = FALSE)
    }
  }

  devices <- device
  saved_files <- character(length(devices))

  for (i in seq_along(devices)) {
    dev <- devices[i]
    current_filename <- paste0(base_filename, ".", dev)

    # --- 3. Handle file overwriting ---
    if (file.exists(current_filename)) {
      if (overwrite_action == "stop") {
        stop("File '", current_filename, "' already exists.", call. = FALSE)
      } else if (overwrite_action == "unique") {
        current_filename <- unique_filename(current_filename)
      }
      # If "overwrite", do nothing and let ggsave handle it
    }

    # --- 4. Handle data embedding for PNGs ---
    is_png <- tolower(dev) %in% c("png")

    if (is_png && embed_data) {
      # Use our special PNG saving function
      do.call(
        save_png_with_data,
        c(list(filename = current_filename, plot = plot,
               plot_call_str = plot_call_str, creator = creator),
          ggsave_args)
      )
    } else {
      # Use the original ggsave for all other cases
      ggsave_args_final <- c(list(filename = current_filename, plot = plot, device = dev), ggsave_args)
      # Special handling for certain devices if necessary
      if (dev %in% c("jpg", "jpeg")) {
        ggsave_args_final$device <- "jpeg"
      } else if (dev == "tiff") {
        ggsave_args_final$device <- "tiff"
      } else if (dev == "eps") {
        ggsave_args_final$device <- "eps"
      }
      do.call(ggplot2::ggsave, ggsave_args_final)
    }

    saved_files[i] <- current_filename
  }

  invisible(saved_files)
}
