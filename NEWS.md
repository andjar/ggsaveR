# ggsaveR (development version)

## New features

* Formats of the same device no longer overwrite each other: when two
  formats would write to the same file, the size is added to the file
  names, as in `plot_120x80mm_300dpi.png`. The new option
  `ggsaveR.size_suffix` adds the size to every file (`TRUE`), only on clashes
  (`"auto"`, the default) or never (`FALSE`), and a format can carry its own
  `suffix`.
* New option `ggsaveR.variants` saves greyscale and simulated colour vision
  deficiency (deutan, protan, tritan) versions of each plot as PNG files.
  The new function `preview_variants()` shows them side by side for
  interactive checks. The simulations use the colorspace package.
* New option `ggsaveR.dir` saves all plots with a relative file name into
  one folder, created if needed.
* New option `ggsaveR.log` appends a row per saved file, with size,
  resolution, MD5 checksum and plot expression, to a CSV file.
* New option `ggsaveR.embed_max_size` (default 5 MB) leaves out the plot
  object and data when they would make a PNG file too large.
* New function `ggsaveR_options()` sets and checks the options and returns
  the previous values.
* `ggsave()` gains the `path` argument of `ggplot2::ggsave()`. Previously it
  was passed through, so the overwrite checks looked at the wrong location.

## Breaking changes

* Embedded data are now stored in separate PNG text chunks: the call and
  session information as plain text, the data as CSV, and only the plot
  object as a serialized R object. As a result:
  * `read_ggsaveR_data()` restores the plot object only with
    `trusted = TRUE`, because unserializing can run arbitrary code. The
    call, data and session information are always safe to read, also from
    other languages.
  * `session_info` is the printed `sessionInfo()` as a string, and
    `plot_data` is read back from CSV. Use `plot_object$data` for the exact
    data.
  * Files written by ggsaveR 0.1.0 can still be read with `trusted = TRUE`.

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

## Other improvements

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
