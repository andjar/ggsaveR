# Getting started with ggsaveR

ggsaveR replaces
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
with a version whose extra behaviour is switched on with global options.
The options are set once, at the top of a script, and apply to every
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md) call
that follows. Existing code does not need to change.

``` r

library(ggplot2)
library(ggsaveR)
#> 
#> Attaching package: 'ggsaveR'
#> The following object is masked from 'package:ggplot2':
#> 
#>     ggsave

p <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  labs(x = "Fuel economy (mpg)", y = "Weight (1000 lb)", colour = "Cylinders")
```

Without any options,
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
behaves as in ggplot2. It returns the paths of the files it wrote,
invisibly:

``` r

ggsave("plain.png", p, width = 5, height = 3.5)
```

## Setting options

[`ggsaveR_options()`](https://andjar.github.io/ggsaveR/reference/ggsaveR_options.md)
sets one or more options, checks them, and returns the previous values
so they can be restored:

``` r

old <- ggsaveR_options(formats = c("png", "pdf"))
getOption("ggsaveR.formats")
#> [1] "png" "pdf"
options(old)
```

The same options can be set with
[`options()`](https://rdrr.io/r/base/options.html) by prefixing them
with `ggsaveR.`, for example
`options(ggsaveR.formats = c("png", "pdf"))`. Calling
[`ggsaveR_options()`](https://andjar.github.io/ggsaveR/reference/ggsaveR_options.md)
without arguments lists the current values.

## Several formats and sizes

The `formats` option turns one
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md) call
into several files. The extension of `filename` is replaced by that of
each format.

``` r

old <- ggsaveR_options(formats = c("png", "pdf"))
print(ggsave("economy.png", p, width = 5, height = 3.5))
#> [1] "economy.png" "economy.pdf"
options(old)
```

Each format can also have its own size, resolution and other
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
arguments. Settings in a format take precedence over arguments given in
the call, and arguments in the call fill in whatever a format leaves
out.

``` r

old <- ggsaveR_options(formats = list(
  list(device = "png", width = 180, height = 120, units = "mm", dpi = 300),
  list(device = "pdf", width = 180, height = 120, units = "mm")
))
print(ggsave("economy.png", p))
#> [1] "economy.png" "economy.pdf"
options(old)
```

### File names that describe the size

If two formats would write to the same file, such as a PNG for a journal
and a PNG for slides, ggsaveR adds the size to their names:

``` r

old <- ggsaveR_options(formats = list(
  list(device = "png", width = 85, height = 60, units = "mm", dpi = 600),
  list(device = "png", width = 254, height = 143, units = "mm", dpi = 150)
))
print(ggsave("economy.png", p))
#> [1] "economy_85x60mm_600dpi.png"   "economy_254x143mm_150dpi.png"
options(old)
```

The size is the one ggplot2 actually uses, after `scale` and default
dimensions are applied. Raster formats also include the resolution. With
`size_suffix = TRUE` every file gets a size suffix; with `FALSE`,
clashing names are an error. A format can also carry its own `suffix`:

``` r

old <- ggsaveR_options(formats = list(
  list(device = "png", width = 85, height = 60, units = "mm", suffix = "_journal"),
  list(device = "png", width = 254, height = 143, units = "mm", suffix = "_slides")
))
print(ggsave("economy.png", p, dpi = 150))
#> [1] "economy_journal.png" "economy_slides.png"
options(old)
```

## Output folder

`dir` places every file saved with a relative name in one folder, which
is created if needed. Absolute paths are left alone.

``` r

old <- ggsaveR_options(dir = "figures")
print(ggsave("economy.png", p, width = 5, height = 3.5))
#> [1] "figures/economy.png"
options(old)
```

## Protecting existing files

By default an existing file is overwritten, as in ggplot2. With
`overwrite_action = "unique"`, ggsaveR adds a number instead:

``` r

old <- ggsaveR_options(overwrite_action = "unique")
ggsave("draft.png", p, width = 5, height = 3.5)
ggsave("draft.png", p, width = 5, height = 3.5)
#> File exists. Saving to draft-1.png instead.
options(old)
```

With `"stop"`, an existing file is an error. When several files are
saved at once, the check covers all of them before anything is written,
and `"unique"` gives them all the same number so they stay together.

## Bypassing ggsaveR for one call

`guard = TRUE` hands the call unchanged to
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
whatever options are set:

``` r

old <- ggsaveR_options(formats = c("png", "pdf"))
ggsave("only-png.png", p, width = 5, height = 3.5, guard = TRUE)
options(old)
```

Scripts that use `guard` need ggsaveR to be loaded;
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
does not know the argument.

## Masking

ggsaveR attaches ggplot2 when it is loaded, so its
[`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md) takes
precedence whichever package is loaded first.
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
always reaches the original.

## Further reading

- [`vignette("colour-checks", package = "ggsaveR")`](https://andjar.github.io/ggsaveR/articles/colour-checks.md):
  greyscale and colour vision deficiency versions of your figures.
- [`vignette("reproducibility", package = "ggsaveR")`](https://andjar.github.io/ggsaveR/articles/reproducibility.md):
  embedding code, data and session information in PNG files, and logging
  saved figures.
