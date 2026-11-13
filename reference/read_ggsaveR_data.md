# Read reproducibility data embedded in a PNG

Extracts the information that
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
embeds in a PNG file when the `ggsaveR.embed_data` option is `TRUE`.

## Usage

``` r
read_ggsaveR_data(path, trusted = FALSE)
```

## Arguments

- path:

  Path to a PNG file written by
  [`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md).

- trusted:

  If `TRUE`, also restore the serialized plot object. Files written by
  ggsaveR 0.1.0, which stored everything as one serialized object, can
  only be read with `trusted = TRUE`.

## Value

A list with some or all of these elements, depending on the
`ggsaveR.embed_metadata` option in effect when the file was saved:

- `ggsaveR_version`:

  Version of ggsaveR that wrote the file.

- `plot_call`:

  The expression passed as `plot`, as a string.

- `session_info`:

  The printed
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html), as a
  string.

- `plot_data`:

  The plot data as a data frame, read back from CSV. Column types are
  those [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html)
  infers; factor levels, dates and row names are not kept. Use
  `plot_object$data` for the exact data.

- `plot_object`:

  The plot object, only with `trusted = TRUE`.

Returns `NULL`, with a warning, if the file holds no ggsaveR data or
cannot be read.

## Security

The plot object is stored as a serialized R object. Restoring it with
[`unserialize()`](https://rdrr.io/r/base/serialize.html) can run
arbitrary code, so it is only read when you set `trusted = TRUE`. Do
that only for files that you, or someone you trust, created. The call,
session information and data are stored as plain text and CSV, and are
always safe to read.

## Reading the data outside R

The data are stored in the PNG text chunk `ggsaveR_data_csv` as a UTF-8
CSV file, compressed with zlib and base64 encoded. In Python, for
example: `zlib.decompress(base64.b64decode(text))`. The call and session
are in the plain-text chunks `ggsaveR_call` and `ggsaveR_session`, which
tools such as `exiftool` show directly.

## Examples

``` r
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
out <- file.path(tempdir(), "embedded.png")

old <- ggsaveR_options(embed_data = TRUE)
ggsave(out, p, width = 5, height = 4)
#> Embedded reproducibility data into embedded.png (304.6 Kb)
options(old)

info <- read_ggsaveR_data(out)
#> 'embedded.png' also holds the plot object. Use `trusted = TRUE` to restore it, but only if you trust the file.
info$plot_call
#> [1] "p"
head(info$plot_data)
#>    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> 6 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# The plot object itself: only for files you trust
restored <- read_ggsaveR_data(out, trusted = TRUE)$plot_object
```
