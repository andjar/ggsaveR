#' Append one row per saved file to the CSV log
#' @noRd
write_log <- function(log, paths, devices, args, plot_call) {
  sizes <- lapply(args, output_size)
  raster <- devices %in% raster_devices

  rows <- data.frame(
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file = paths,
    device = devices,
    width = vapply(sizes, function(s) round(s$width, 2), numeric(1)),
    height = vapply(sizes, function(s) round(s$height, 2), numeric(1)),
    units = vapply(sizes, function(s) s$units, character(1)),
    dpi = ifelse(raster, vapply(sizes, function(s) s$dpi, numeric(1)), NA),
    bytes = file.size(paths),
    md5 = unname(tools::md5sum(paths)),
    plot = gsub("\\s*\n\\s*", " ", plot_call),
    stringsAsFactors = FALSE
  )

  log_dir <- dirname(log)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  is_new <- !file.exists(log)
  utils::write.table(
    rows, log,
    sep = ",", row.names = FALSE, col.names = is_new, append = !is_new,
    qmethod = "double", na = "", fileEncoding = "UTF-8"
  )
  invisible(log)
}
