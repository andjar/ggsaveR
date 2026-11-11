# Read reproducibility data embedded in a PNG

Extracts the data that
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
embeds in a PNG file when the `ggsaveR.embed_data` option is `TRUE`.

## Usage

``` r
read_ggsaveR_data(path)
```

## Arguments

- path:

  Path to a PNG file written by
  [`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md).

## Value

A list with some or all of the elements `ggsaveR_version`,
`plot_object`, `plot_data`, `session_info` and `plot_call`, depending on
the `ggsaveR.embed_metadata` option in effect when the file was saved.
Returns `NULL`, with a warning, if the file holds no ggsaveR data or
cannot be read.

## Security

The data are stored as a serialized R object and restored with
[`unserialize()`](https://rdrr.io/r/base/serialize.html). Unserializing
a file from an untrusted source can execute arbitrary code, so only use
this function on PNG files that you, or someone you trust, created.

## Examples

``` r
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
out <- file.path(tempdir(), "embedded.png")

old <- options(ggsaveR.embed_data = TRUE)
ggsave(out, p, width = 5, height = 4)
#> Embedded reproducibility data into embedded.png (278.6 Kb)
options(old)

info <- read_ggsaveR_data(out)
names(info)
#> [1] "ggsaveR_version" "plot_object"     "plot_data"       "session_info"   
#> [5] "plot_call"      
info$plot_call
#> [1] "p"
head(info$plot_data)
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```
