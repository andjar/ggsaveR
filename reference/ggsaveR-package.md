# ggsaveR: a drop-in `ggsave()` with extra features

ggsaveR masks
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
with a version whose extra behaviour is controlled entirely by global
options. Load the package and set the options at the top of a script,
and every existing
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md) call
picks them up.

## Options

- `ggsaveR.formats`:

  `NULL` (default), a character vector of devices, or a list of lists
  that each hold a `device` and optional
  [`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
  arguments such as `width`, `height`, `units` and `dpi`. One file is
  written per format.

- `ggsaveR.overwrite_action`:

  `"overwrite"` (default), `"unique"` or `"stop"`.

- `ggsaveR.embed_data`:

  `FALSE` (default) or `TRUE`. Embed reproducibility data in PNG files;
  see
  [`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md).

- `ggsaveR.embed_metadata`:

  Which parts to embed: any of `"plot"`, `"data"`, `"session_info"` and
  `"call"` (default: all).

- `ggsaveR.creator`:

  `NULL` (default) or a string stored as the `Author` text field of PNG
  files.

Metadata (`ggsaveR.embed_data` and `ggsaveR.creator`) is only written to
PNG files; other formats are saved unchanged.

## Masking

ggsaveR attaches ggplot2 when it is loaded, so
[`ggsaveR::ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
masks
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
regardless of the order of the
[`library()`](https://rdrr.io/r/base/library.html) calls. Use
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
or `ggsave(..., guard = TRUE)` to reach the original.

## See also

Useful links:

- <https://andjar.github.io/ggsaveR/>

- <https://github.com/andjar/ggsaveR>

- Report bugs at <https://github.com/andjar/ggsaveR/issues>

## Author

**Maintainer**: Anders Hagen Jarmund <anders.h.jarmund@ntnu.no>
