# Save a ggplot (or other grid object) with ggsaveR enhancements

A drop-in replacement for
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
Called without any `ggsaveR.*` options set, it behaves like the
original. Setting options switches on extra behaviour for every
`ggsave()` call in a script: saving to several formats at once,
protecting existing files, and embedding metadata in PNG files.

## Usage

``` r
ggsave(filename, plot = last_plot(), device = NULL, ..., guard = FALSE)
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

All enhancements are controlled with
[`options()`](https://rdrr.io/r/base/options.html); see
[ggsaveR-package](https://andjar.github.io/ggsaveR/reference/ggsaveR-package.md)
for the full reference.

- `ggsaveR.formats`: save every plot in several formats and sizes.
  Either a character vector of devices (`c("png", "pdf")`) or a list of
  lists, each with a `device` and any other `ggsave()` arguments
  (`list(device = "png", width = 8, height = 5, dpi = 300)`). The
  extension of `filename` is replaced, and the `device` argument is
  ignored. Settings in a format take precedence over arguments given in
  the call.

- `ggsaveR.overwrite_action`: what to do when a file already exists:
  `"overwrite"` (default), `"stop"` (error, before anything is written)
  or `"unique"` (save as `plot-1.png`, `plot-2.png`, ...; all formats of
  one call share the same suffix).

- `ggsaveR.embed_data`: if `TRUE`, embed the plot object, its data, the
  call and the session info in PNG files. Read them back with
  [`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md).
  `ggsaveR.embed_metadata` selects which parts.

- `ggsaveR.creator`: a string stored as the `Author` of PNG files.

## Differences from ggplot2

- `device` may be a character vector, in which case one file is written
  per device and the extension of `filename` is replaced.

- If `filename` has no extension and `device` is a single string, the
  device is appended as the extension.

- A character vector of saved paths is returned (invisibly), rather than
  the single `filename`.

## Embedded data and privacy

With `ggsaveR.embed_data = TRUE` the PNG contains the **raw data**
behind the plot (all columns, not only the plotted ones) and, for plots
built inside a function, every object in that function's environment. Do
not share such files if the data are sensitive, and expect larger files.
Use `ggsaveR.embed_metadata` to embed less, or `guard = TRUE` to skip a
single plot.

## See also

[`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md)
to read embedded data back,
[ggsaveR-package](https://andjar.github.io/ggsaveR/reference/ggsaveR-package.md)
for the options.

## Examples

``` r
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
out <- file.path(tempdir(), "mtcars.png")

# Works like ggplot2::ggsave()
ggsave(out, p, width = 5, height = 4)

# Save every plot as both PNG and PDF, and never overwrite
old <- options(
  ggsaveR.formats = c("png", "pdf"),
  ggsaveR.overwrite_action = "unique"
)
ggsave(out, p, width = 5, height = 4)
#> File exists. Saving to mtcars-1.png, mtcars-1.pdf instead.

# Bypass the options for one call
ggsave(out, p, width = 5, height = 4, guard = TRUE)

options(old)
```
