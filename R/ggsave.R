#' @importFrom rlang call_args `%||%`
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom tools file_path_sans_ext file_ext

# You will need to define these helper functions elsewhere in your package
# For demonstration, here are plausible implementations:
unique_filename <- function(path) {
  if (!file.exists(path)) return(path)
  base <- tools::file_path_sans_ext(path)
  ext <- tools::file_ext(path)
  i <- 1
  repeat {
    new_path <- sprintf("%s-%d.%s", base, i, ext)
    if (!file.exists(new_path)) return(new_path)
    i <- i + 1
  }
}

# save_png_with_data would be a complex function that uses png::writePNG
# and embeds metadata in tEXt chunks. This is a placeholder.
save_png_with_data <- function(filename, plot, plot_call_str, creator, ...) {
  # In a real implementation, you would:
  # 1. Create a list of data to embed (plot, plot_call_str, sessioninfo, etc.)
  # 2. Serialize this list (e.g., using saveRDS)
  # 3. Base64 encode it to be safe for text chunks.
  # 4. Use a low-level PNG writer to save the plot and inject the data
  #    as a tEXt or zTXt chunk with a custom keyword like "ggsaveR_data".
  # 5. Inject creator into the "Author" tEXt chunk if provided.
  # For now, we just call the original ggsave as a placeholder.
  message(
    "Data embedding for '", filename, "' is enabled.",
    "\nPlot Call: ", plot_call_str
  )
  ggsave_args_final <- c(list(filename = filename, plot = plot, device = "png"), list(...))
  if (!is.null(creator)) {
    # The 'author' argument for ggsave is passed to the device,
    # but png() doesn't have it. We would handle it manually here.
    # For other devices, the main function handles it.
  }
  do.call(ggplot2::ggsave, ggsave_args_final)
}


#' Save a ggplot with Supercharged Features
#'
#' This function is a wrapper for [ggplot2::ggsave()] that adds powerful
#' new features controlled entirely via [options()], requiring no changes to
#' existing code that calls `ggsave()`.
#'
#' All enhancements are controlled by setting `options()` before calling `ggsave()`.
#'
#' @section New Features via Options:
#'
#' *   **Multiple Formats/Dimensions (`ggsaveR.formats`):**
#'     To save a plot in multiple formats and sizes with a single `ggsave()` call,
#'     set this option to a list of configurations. Each configuration is a list
#'     specifying `device`, and optionally `width`, `height`, `units`, `dpi`, etc.
#'     If this option is set, it overrides the `device` argument and the filename
#'     extension of the `ggsave()` call.
#'     *Example:* `options(ggsaveR.formats = list(list(device = "png", dpi = 300), list(device = "pdf", width = 8, height = 6)))`
#'
#' *   **File Overwriting (`ggsaveR.overwrite_action`):**
#'     Controls behavior when a file already exists.
#'     - `"overwrite"`: (Default) Replaces the existing file.
#'     - `"unique"`: Appends a number to create a unique filename (e.g., `plot-1.png`).
#'     - `"stop"`: Throws an error and stops execution.
#'
#' *   **Data Embedding (`ggsaveR.embed_data`):**
#'     If `TRUE`, the ggplot object, the code used to generate it, and session
#'     information are embedded into the PNG file's metadata for reproducibility.
#'     (Applies to PNG format only).
#'
#' *   **Creator Metadata (`ggsaveR.creator`):**
#'     A character string (e.g., `"Your Name"`) to be embedded as the author in
#'     the metadata of supported file types (e.g., PDF, PNG).
#'
#' @param filename File name to create on disk. The extension is used only if
#'   the `ggsaveR.formats` option is NOT set. Otherwise, this is used as the
#'   base name for the files.
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param device Device to use. Can be a vector of devices (e.g., `c("png", "pdf")`).
#'   This argument is **ignored** if the `ggsaveR.formats` option is set.
#' @param guard A logical value. If `TRUE`, all `ggsaveR` enhancements are
#'   bypassed, and the call is forwarded directly to [ggplot2::ggsave()]. This
#'   provides an "escape hatch" for specific plots. See Warning section.
#' @param ... Other arguments passed on to [ggplot2::ggsave()].
#'
#' @return Invisibly returns a character vector of the saved file paths.
#'
#' @section Warning:
#' Using the `guard = TRUE` argument requires the `ggsaveR` package to be loaded.
#' If you run a script containing `ggsave(..., guard = TRUE)` in an R session
#' where `ggsaveR` is not loaded, your code will fail with an "unused argument"
#' error from the original [ggplot2::ggsave()].
#'
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # library(ggsaveR) # This would be run by the user
#'
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' # --- Example 1: Multi-format/dimension via options (THE NEW WAY) ---
#' # The user's code just calls ggsave() as always.
#' # The magic happens in the options.
#' options(ggsaveR.formats = list(
#'   list(device = "png", width = 8, height = 6, units = "in", dpi = 300),
#'   list(device = "pdf", width = 7, height = 7, units = "in"),
#'   list(device = "svg", width = 15, height = 12, units = "cm")
#' ))
#'
#' # This single call creates "my_plot.png", "my_plot.pdf", and "my_plot.svg"
#' # with the dimensions specified in the options.
#' saved_files <- ggsave("my_plot.png", p) # extension is ignored
#' print(saved_files)
#'
#' # Reset the option to return to default behavior
#' options(ggsaveR.formats = NULL)
#'
#'
#' # --- Example 2: Using the guard argument ---
#' # Let's set an option we want to bypass for one specific plot
#' options(ggsaveR.overwrite_action = "unique")
#' ggsave("my_plot.png", p) # Saves "my_plot.png"
#' ggsave("my_plot.png", p) # Saves "my_plot-1.png"
#'
#' # Now, let's force an overwrite for this next call using the guard.
#' # This will call ggplot2::ggsave directly, which overwrites by default.
#' ggsave("my_plot.png", p, guard = TRUE) # Overwrites "my_plot.png"
#'
#' # Reset options
#' options(ggsaveR.overwrite_action = "overwrite")
#'
#'
#' # --- Example 3: Combining options ---
#' options(
#'   ggsaveR.formats = list(list(device = "png"), list(device = "pdf")),
#'   ggsaveR.embed_data = TRUE, # Will apply to the PNG
#'   ggsaveR.creator = "Dr. G. G. Plotter"
#' )
#'
#' ggsave("final_plot", p) # creates final_plot.png and final_plot.pdf
#'
#' # Reset all options
#' options(
#'   ggsaveR.formats = NULL,
#'   ggsaveR.embed_data = FALSE,
#'   ggsaveR.creator = NULL
#' )
#' }
ggsave <- function(filename, plot = last_plot(), device = NULL, ..., guard = FALSE) {

  # --- 0. Guard clause to bypass all ggsaveR enhancements ---
  if (isTRUE(guard)) {
    # Capture the original call
    call <- match.call()
    # Remove the `guard` argument so ggplot2::ggsave doesn't see it
    call$guard <- NULL
    # Change the function to be called to the original ggsave
    call[[1]] <- quote(ggplot2::ggsave)
    # Evaluate the modified call in the parent environment to ensure
    # objects are found correctly.
    return(eval.parent(call))
  }

  # --- Capture the plot argument expression for data embedding ---
  plot_arg_expr <- rlang::call_args(match.call())$plot
  plot_call_str <- paste(deparse(plot_arg_expr), collapse = "\n")

  # --- 1. Get all ggsaveR options ---
  formats <- getOption("ggsaveR.formats", NULL)
  overwrite_action <- getOption("ggsaveR.overwrite_action", "overwrite")
  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  creator <- getOption("ggsaveR.creator", NULL)

  # --- Prepare base arguments for all underlying save calls ---
  user_args <- list(...)
  # Inject creator/author if set in options and not already specified by user
  if (!is.null(creator) && !("author" %in% names(user_args))) {
    user_args$author <- creator
  }

  saved_files <- character()
  base_filename <- tools::file_path_sans_ext(filename)

  # --- 2. Main Logic: Check if multi-format option is set ---
  if (is.list(formats) && !is.data.frame(formats)) {
    # --- A. NEW BEHAVIOR: Use ggsaveR.formats option ---
    # This block ignores the `device` argument and filename extension.

    for (fmt_config in formats) {
      dev <- fmt_config$device
      if (is.null(dev)) {
        warning("A format in `ggsaveR.formats` is missing a 'device'. Skipping.", call. = FALSE)
        next
      }

      current_filename <- paste0(base_filename, ".", dev)

      # Handle file overwriting
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") {
          stop("File '", current_filename, "' already exists.", call. = FALSE)
        } else if (overwrite_action == "unique") {
          current_filename <- unique_filename(current_filename)
        }
      }

      # Combine user-provided `...` args with format-specific args
      # Format-specific args will overwrite user `...` args if they conflict
      # (e.g., width in option overrides width in `...`)
      final_args <- c(list(filename = current_filename, plot = plot, device = dev), user_args, fmt_config)
      final_args$device <- dev # Ensure device from fmt_config is used

      # Handle data embedding for PNGs
      if (tolower(dev) == "png" && embed_data) {
        do.call(
          save_png_with_data,
          c(list(plot_call_str = plot_call_str, creator = creator), final_args)
        )
      } else {
        do.call(ggplot2::ggsave, final_args)
      }

      saved_files <- c(saved_files, current_filename)
    }

  } else {
    # --- B. FALLBACK BEHAVIOR: Use `device` argument as in original ggplot2 ---
    # This is the logic from your original function, slightly adapted.

    devices <- device
    if (is.null(devices)) {
      devices <- tolower(tools::file_ext(filename))
      if (devices == "") {
        stop("Cannot determine device from filename with no extension. ",
             "Please specify `device` or use the `ggsaveR.formats` option.", call. = FALSE)
      }
    }

    for (dev in devices) {
      current_filename <- paste0(base_filename, ".", dev)

      # Handle file overwriting
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") {
          stop("File '", current_filename, "' already exists.", call. = FALSE)
        } else if (overwrite_action == "unique") {
          current_filename <- unique_filename(current_filename)
        }
      }

      final_args <- c(list(filename = current_filename, plot = plot, device = dev), user_args)

      # Handle data embedding for PNGs
      if (tolower(dev) == "png" && embed_data) {
        do.call(
          save_png_with_data,
          c(list(plot_call_str = plot_call_str, creator = creator), final_args)
        )
      } else {
        do.call(ggplot2::ggsave, final_args)
      }

      saved_files <- c(saved_files, current_filename)
    }
  }

  invisible(saved_files)
}
