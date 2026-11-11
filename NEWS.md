# ggsaveR (development version)

## Bug fixes

* PNG files with metadata (`ggsaveR.creator` or `ggsaveR.embed_data`) are now
  rendered by `ggplot2::ggsave()` itself and are pixel-identical to files saved
  without metadata. Previously `scale`, `limitsize`, string values of `dpi`
  (such as `"retina"`) and the plot's own background were ignored or failed,
  the default size was 7 x 7 in rather than the size of the current device, and
  the resolution was not recorded in the file.
* An argument given both in the `ggsave()` call and in `ggsaveR.formats` (e.g.
  `width`) no longer fails with "matched by multiple actual arguments". The
  value in the format is used.
* `device` can again be a function, as in `ggplot2::ggsave()`.
* For a single file, `filename` is used as given. `ggsave("Plot.PNG")` is no
  longer renamed to `Plot.png`, and `ggsave("my.plot", device = "png")` no
  longer writes `my.png`.
* `ggsaveR.overwrite_action = "unique"` no longer adds a trailing dot to file
  names without an extension.
* When `ggsave()` is called through `do.call()`, the recorded call is
  `<plot object>` rather than the deparsed plot object.

## Improvements

* ggsaveR now attaches ggplot2 (it is in `Depends`), so its `ggsave()` masks
  the original whatever the order of the `library()` calls. Previously,
  calling `library(ggplot2)` after `library(ggsaveR)` silently disabled all
  enhancements.
* `ggsaveR.formats` can be a character vector of devices, e.g.
  `c("png", "pdf")`.
* With several formats, `"unique"` gives all files of one call the same
  suffix, and `"stop"` fails before any file is written.
* Invalid option values now give an error instead of being silently ignored.
* The message about embedded data reports its size.
* The creation time is stored under the standard PNG keyword `Creation Time`
  (was `CreationTime`).
* `ragg` and `svglite` are now suggested rather than imported, and `rlang` is
  no longer needed. Install `svglite` to save SVG files.
* Vignettes are now built with the package; added examples, a package help
  page (`?ggsaveR`) and this NEWS file.

# ggsaveR 0.1.0

* Initial version.
