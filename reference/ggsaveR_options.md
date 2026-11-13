# Set or inspect ggsaveR options

A convenience wrapper around
[`options()`](https://rdrr.io/r/base/options.html) for the `ggsaveR.*`
options. The new values are checked straight away, so a typo fails here
rather than at the first
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md) call.
Setting an option to `NULL` restores its default.

## Usage

``` r
ggsaveR_options(
  formats,
  size_suffix,
  dir,
  overwrite_action,
  variants,
  embed_data,
  embed_metadata,
  embed_max_size,
  creator,
  log
)
```

## Arguments

- formats:

  Save every plot in several formats and/or sizes. `NULL` (default), a
  character vector of devices such as `c("png", "pdf")`, or a list of
  lists, each with a `device` and any other
  [`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
  arguments, for example
  `list(device = "png", width = 120, height = 80, units = "mm", dpi = 300)`.
  A format may also hold a `suffix`, such as `"_slide"`, that is added
  to the file name.

- size_suffix:

  Whether to add the size to file names, as in
  `plot_120x80mm_300dpi.png`. `"auto"` (default) adds it only where two
  formats would otherwise write to the same file; `TRUE` always adds it;
  `FALSE` never does.

- dir:

  A directory for all plots. Relative file names are saved inside it,
  and it is created if needed. `NULL` (default) uses file names as
  given.

- overwrite_action:

  What to do when a file already exists: `"overwrite"` (default),
  `"unique"` (add `-1`, `-2`, ... to the name) or `"stop"` (error before
  anything is written).

- variants:

  Also save PNG copies that show how the plot looks without colour or to
  readers with a colour vision deficiency. Any of `"greyscale"` (or
  `"grayscale"`), `"deutan"`, `"protan"` and `"tritan"`. The colour
  vision simulations need the colorspace package. See
  [`preview_variants()`](https://andjar.github.io/ggsaveR/reference/preview_variants.md).

- embed_data:

  If `TRUE`, embed reproducibility information in PNG files. See
  [`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md).

- embed_metadata:

  Which parts to embed: any of `"plot"`, `"data"`, `"session_info"` and
  `"call"` (default: all).

- embed_max_size:

  Largest size, in bytes, of the embedded plot object and data in one
  file (default 5 MB). Above it they are left out with a warning, while
  the call and session information are still embedded. Use `Inf` for no
  limit.

- creator:

  A string stored as the `Author` of PNG files.

- log:

  Path to a CSV file. Every saved file is appended to it as a row with
  time, path, size, resolution, MD5 checksum and the plot expression.

## Value

With arguments, the previous values of the options that were changed,
invisibly, in a form that can be passed to
[`options()`](https://rdrr.io/r/base/options.html). Without arguments, a
named list of the current values of all ggsaveR options.

## Examples

``` r
old <- ggsaveR_options(formats = c("png", "pdf"), overwrite_action = "unique")
ggsaveR_options()
#> $ggsaveR.formats
#> [1] "png" "pdf"
#> 
#> $ggsaveR.size_suffix
#> [1] "auto"
#> 
#> $ggsaveR.dir
#> NULL
#> 
#> $ggsaveR.overwrite_action
#> [1] "unique"
#> 
#> $ggsaveR.variants
#> NULL
#> 
#> $ggsaveR.embed_data
#> [1] FALSE
#> 
#> $ggsaveR.embed_metadata
#> [1] "plot"         "data"         "session_info" "call"        
#> 
#> $ggsaveR.embed_max_size
#> [1] 5e+06
#> 
#> $ggsaveR.creator
#> NULL
#> 
#> $ggsaveR.log
#> NULL
#> 
options(old)
```
