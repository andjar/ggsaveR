variant_labels <- c(
  greyscale = "Greyscale",
  grayscale = "Grayscale",
  deutan = "Deuteranopia (deutan)",
  protan = "Protanopia (protan)",
  tritan = "Tritanopia (tritan)"
)

# Only the arguments that every raster device understands
size_args <- c("width", "height", "units", "dpi", "scale", "bg", "limitsize")

#' Render a plot to a PNG array, using ggplot2::ggsave()
#' @noRd
render_png <- function(plot, args) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  do.call(
    ggplot2::ggsave,
    c(list(filename = tmp, plot = plot, device = "png"), args[intersect(names(args), size_args)])
  )
  png::readPNG(tmp, info = TRUE)
}

#' Save greyscale and colour vision deficiency versions of a plot
#' @noRd
save_variants <- function(plot, args, paths, variants) {
  img <- render_png(plot, args)
  dpi <- attr(img, "info")$dpi
  for (i in seq_along(variants)) {
    png::writePNG(convert_image(img, variants[[i]]), target = paths[[i]], dpi = dpi)
  }
  invisible(paths)
}

#' Convert an image array as read by png::readPNG()
#'
#' Greyscale keeps the relative luminance of each pixel, computed in linear
#' RGB, which is how the colours will differ in a greyscale print. The colour
#' vision deficiency variants use the simulations of Machado et al. (2009) as
#' implemented in the colorspace package, at full severity.
#' @noRd
convert_image <- function(img, variant) {
  channels <- dim(img)[3]
  if (is.na(channels) || channels < 3L) {
    return(img)  # already grey
  }
  attributes(img) <- list(dim = dim(img))
  rgb <- img[, , 1:3, drop = FALSE]

  if (variant %in% c("greyscale", "grayscale")) {
    lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    y <- 0.2126 * lin[, , 1] + 0.7152 * lin[, , 2] + 0.0722 * lin[, , 3]
    grey <- ifelse(y <= 0.0031308, 12.92 * y, 1.055 * y^(1 / 2.4) - 0.055)
    for (k in 1:3) img[, , k] <- grey
    return(img)
  }

  # Simulate once per distinct colour; plots have few of them
  hex <- grDevices::rgb(rgb[, , 1], rgb[, , 2], rgb[, , 3])
  colours <- unique(hex)
  simulate <- switch(variant,
    deutan = colorspace::deutan,
    protan = colorspace::protan,
    tritan = colorspace::tritan
  )
  new <- grDevices::col2rgb(simulate(colours)) / 255
  idx <- match(hex, colours)
  for (k in 1:3) img[, , k] <- new[k, idx]
  img
}

#' Preview a plot in greyscale and as seen with colour vision deficiencies
#'
#' Draws the plot next to versions that show how it looks when printed in
#' greyscale and to readers with the three main types of colour vision
#' deficiency. Use it to check that groups stay distinguishable without
#' relying on colour alone. To save these versions alongside every plot, set
#' the `ggsaveR.variants` option instead.
#'
#' Greyscale uses the relative luminance of each colour. The colour vision
#' deficiency simulations (Machado, Oliveira and Fernandes, 2009) come from
#' the colorspace package and show complete deuteranopia, protanopia and
#' tritanopia; milder forms are more common.
#'
#' @param plot Plot to check, defaults to the last plot displayed.
#' @param variants Any of `"greyscale"` (or `"grayscale"`), `"deutan"`,
#'   `"protan"` and `"tritan"`.
#' @param width,height,units,dpi Size at which the plot is rendered. Use the
#'   size of the final figure so that text and point sizes look right.
#' @param draw If `TRUE`, draw the comparison on the current device.
#' @param ... Other arguments passed on to [ggplot2::ggsave()], such as `bg`.
#' @return Invisibly, a grob with the original and the variants side by
#'   side, which can be drawn with [grid::grid.draw()] or saved with
#'   [ggsave()].
#' @references Machado GM, Oliveira MM, Fernandes LAF (2009). A
#'   physiologically-based model for simulation of color vision deficiency.
#'   *IEEE Transactions on Visualization and Computer Graphics*, 15(6),
#'   1291-1298. \doi{10.1109/TVCG.2009.113}
#' @examples
#' library(ggplot2)
#' p <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
#' preview_variants(p, variants = "greyscale", width = 5, height = 3.5)
#'
#' if (requireNamespace("colorspace", quietly = TRUE)) {
#'   preview_variants(p, width = 5, height = 3.5)
#' }
#' @export
preview_variants <- function(plot = last_plot(),
                             variants = c("greyscale", "deutan", "protan", "tritan"),
                             width = 6, height = 4, units = "in", dpi = 100,
                             draw = TRUE, ...) {
  variants <- check_variants(variants)
  img <- render_png(plot, list(width = width, height = height, units = units, dpi = dpi, ...))

  panels <- c(list(img), lapply(variants, convert_image, img = img))
  labels <- c("Original", unname(variant_labels[variants]))
  grob <- panel_grid(panels, labels)

  if (isTRUE(draw)) {
    grid::grid.newpage()
    grid::grid.draw(grob)
  }
  invisible(grob)
}

#' Arrange images in a grid, each with a title above it
#' @noRd
panel_grid <- function(images, labels) {
  n <- length(images)
  ncol <- if (n <= 3L) n else ceiling(sqrt(n))
  nrow <- ceiling(n / ncol)
  heights <- rep(grid::unit.c(grid::unit(1.6, "lines"), grid::unit(1, "null")), nrow)
  layout <- grid::grid.layout(nrow * 2L, ncol, heights = heights)

  children <- list()
  for (i in seq_len(n)) {
    row <- (i - 1L) %/% ncol
    col <- (i - 1L) %% ncol + 1L
    children[[length(children) + 1L]] <- grid::textGrob(
      labels[[i]],
      gp = grid::gpar(fontsize = 11, fontface = "bold"),
      vp = grid::viewport(layout.pos.row = 2L * row + 1L, layout.pos.col = col)
    )
    children[[length(children) + 1L]] <- grid::rasterGrob(
      images[[i]],
      vp = grid::viewport(layout.pos.row = 2L * row + 2L, layout.pos.col = col)
    )
  }
  grid::gTree(
    children = do.call(grid::gList, children),
    vp = grid::viewport(layout = layout),
    cl = "ggsaveR_variants"
  )
}
