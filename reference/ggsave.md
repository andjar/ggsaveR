# Save a ggplot (or other grid object) with ggsaveR enhancements

A drop-in replacement for
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
Called without any `ggsaveR.*` options set, it behaves like the
original. Setting options switches on extra behaviour for every
`ggsave()` call in a script. See
[`ggsaveR_options()`](https://andjar.github.io/ggsaveR/reference/ggsaveR_options.md)
for the full list.

## Usage

``` r
ggsave(
  filename,
  plot = last_plot(),
  device = NULL,
  path = NULL,
  ...,
  guard = FALSE
)
```

## Arguments

- filename:

  File name to create on disk.

- plot:

  Plot to save, defaults to the last plot displayed.

- device:

  Device to use: `NULL` (guess from the extension of `filename`), a
  device name such as `"png"`, a character vector of device names, or a
  device function. See
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
  Ignored when the `ggsaveR.formats` option is set.

- path:

  Directory to save the plot in, combined with `filename` as in
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- ...:

  Other arguments passed on to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
  such as `width`, `height`, `units`, `dpi`, `scale` or `bg`.

- guard:

  If `TRUE`, all ggsaveR enhancements are bypassed and the call is
  forwarded unchanged to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

Invisibly, a character vector with the paths of the files created.

## Options

- `ggsaveR.formats`: save every plot in several formats and sizes,
  either as a character vector of devices (`c("png", "pdf")`) or as a
  list of lists, each with a `device` and any other `ggsave()` arguments
  (`list(device = "png", width = 120, height = 80, units = "mm", dpi = 300)`).
  The extension of `filename` is replaced and the `device` argument is
  ignored. Settings in a format take precedence over arguments in the
  call.

- `ggsaveR.size_suffix`: add the size to file names, as in
  `plot_120x80mm_300dpi.png`. By default (`"auto"`) this happens only
  when two formats would otherwise write to the same file.

- `ggsaveR.dir`: save all plots with a relative file name into this
  directory.

- `ggsaveR.overwrite_action`: `"overwrite"` (default), `"unique"` or
  `"stop"` when a file already exists.

- `ggsaveR.variants`: also save greyscale and colour vision deficiency
  versions as PNG files, such as `plot_greyscale.png` and
  `plot_deutan.png`.

- `ggsaveR.embed_data`, `ggsaveR.embed_metadata`,
  `ggsaveR.embed_max_size`: embed the plot expression, data, session
  information and plot object in PNG files. Read them back with
  [`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md).

- `ggsaveR.creator`: a string stored as the `Author` of PNG files.

- `ggsaveR.log`: append a row describing every saved file to a CSV file.

## File names

The final file name is built in this order: the directory from
`ggsaveR.dir` (for relative names), the stem of `filename`, a suffix (an
explicit `suffix` from the format, otherwise a size suffix if
`ggsaveR.size_suffix` calls for one), the extension of the device, and
finally `-1`, `-2`, ... if `ggsaveR.overwrite_action = "unique"` and the
file exists. Variants add `_greyscale` etc. to the name of the first
format.

## Differences from ggplot2

- `device` may be a character vector, in which case one file is written
  per device and the extension of `filename` is replaced.

- If `filename` has no extension and `device` is a single string, the
  device is appended as the extension.

- A character vector of saved paths is returned (invisibly), rather than
  the single `filename`.

## Embedded data and privacy

With `ggsaveR.embed_data = TRUE` the PNG contains the **raw data**
behind the plot (all columns, not only the plotted ones) and, if
`"plot"` is embedded, the plot object, which for plots built inside a
function includes that function's environment. Do not share such files
if the data are sensitive. Use `ggsaveR.embed_metadata` to embed less,
or `guard = TRUE` to skip a single plot.

## See also

[`ggsaveR_options()`](https://andjar.github.io/ggsaveR/reference/ggsaveR_options.md)
for all options,
[`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md)
to read embedded data back,
[`preview_variants()`](https://andjar.github.io/ggsaveR/reference/preview_variants.md)
to check colours interactively.

## Examples

``` r
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
out <- file.path(tempdir(), "mtcars.png")

# Works like ggplot2::ggsave()
ggsave(out, p, width = 5, height = 4)

# Save every plot as PNG and PDF, and never overwrite
old <- ggsaveR_options(formats = c("png", "pdf"), overwrite_action = "unique")
ggsave(out, p, width = 5, height = 4)
#> File exists. Saving to mtcars-1.png, mtcars-1.pdf instead.
options(old)

# Two sizes of the same format get the size in their file names:
# mtcars_120x80mm_300dpi.png and mtcars_240x160mm_150dpi.png
old <- ggsaveR_options(formats = list(
  list(device = "png", width = 120, height = 80, units = "mm", dpi = 300),
  list(device = "png", width = 240, height = 160, units = "mm", dpi = 150)
))
ggsave(out, p)
options(old)

# Bypass all options for one call
ggsave(out, p, width = 5, height = 4, guard = TRUE)
```
