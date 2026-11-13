# Reproducible figures: embedded data and logging

Figure files tend to outlive the scripts that made them. ggsaveR offers
two ways to keep track of where a figure came from: embedding
information in the PNG file itself, and keeping a log of every file
saved.

``` r

library(ggplot2)
library(ggsaveR)
#> 
#> Attaching package: 'ggsaveR'
#> The following object is masked from 'package:ggplot2':
#> 
#>     ggsave
```

## Embedding data in PNG files

With `embed_data = TRUE`, every PNG file carries the expression passed
as `plot`, the data behind the plot, the printed
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) and the plot
object. The information is stored in the file’s text chunks, so the
image looks the same and opens in any viewer.

``` r

old <- ggsaveR_options(embed_data = TRUE)

ggsave(
  "versicolor-virginica.png",
  iris |>
    subset(Species != "setosa") |>
    ggplot(aes(Sepal.Length, Petal.Length, colour = Species)) +
    geom_point(),
  width = 6, height = 4
)
#> Embedded reproducibility data into versicolor-virginica.png (140.9 Kb)
options(old)
```

[`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md)
reads the information back:

``` r

info <- read_ggsaveR_data("versicolor-virginica.png")
#> 'versicolor-virginica.png' also holds the plot object. Use `trusted = TRUE` to restore it, but only if you trust the file.
cat(info$plot_call)
#> ggplot(subset(iris, Species != "setosa"), aes(Sepal.Length, Petal.Length, 
#>     colour = Species)) + geom_point()
head(info$plot_data)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1          7.0         3.2          4.7         1.4 versicolor
#> 2          6.4         3.2          4.5         1.5 versicolor
#> 3          6.9         3.1          4.9         1.5 versicolor
#> 4          5.5         2.3          4.0         1.3 versicolor
#> 5          6.5         2.8          4.6         1.5 versicolor
#> 6          5.7         2.8          4.5         1.3 versicolor
```

When the plot is stored in a variable, as in `ggsave("plot.png", p)`,
the recorded expression is just `p`. The data and plot object are
embedded either way.

### The plot object

The plot object is stored as a serialized R object. Restoring a
serialized object can run arbitrary code, in the same way as
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) or
[`load()`](https://rdrr.io/r/base/load.html), so
[`read_ggsaveR_data()`](https://andjar.github.io/ggsaveR/reference/read_ggsaveR_data.md)
only restores it when you confirm that you trust the file:

``` r

restored <- read_ggsaveR_data("versicolor-virginica.png", trusted = TRUE)$plot_object
restored + theme_minimal()
```

![](reproducibility_files/figure-html/unnamed-chunk-5-1.png)

The call, session information and data are stored as plain text and CSV,
and are always safe to read. Data read from the CSV copy have the column
types that [`read.csv()`](https://rdrr.io/r/utils/read.table.html)
infers; `restored$data` has the exact original.

### Reading the information outside R

The call and session information are in the text chunks `ggsaveR_call`
and `ggsaveR_session`, which tools such as `exiftool` show directly. The
data are in `ggsaveR_data_csv` as a UTF-8 CSV file, compressed with zlib
and base64 encoded. In Python:

``` python
import base64, io, zlib
import pandas as pd
from PIL import Image

text = Image.open("versicolor-virginica.png").text["ggsaveR_data_csv"]
data = pd.read_csv(io.BytesIO(zlib.decompress(base64.b64decode(text))))
```

### Choosing what to embed

`embed_metadata` selects the parts: any of `"plot"`, `"data"`,
`"session_info"` and `"call"`. To record only how a figure was made,
without its data:

``` r

ggsaveR_options(embed_data = TRUE, embed_metadata = c("call", "session_info"))
```

The plot object and data are left out, with a warning, if together they
would add more than `embed_max_size` bytes (5 MB by default).

### Privacy

A PNG with embedded data contains every column of the plot’s data frame,
including those not shown. For plots built inside a function, the plot
object also carries the objects in that function’s environment. Do not
share such files if the data are sensitive. To save one figure without
embedding, use `guard = TRUE`.

## Author metadata

The `creator` option stores a name as the `Author` field of PNG files,
which file managers and image viewers can display:

``` r

old <- ggsaveR_options(creator = "Jane Doe, University of Somewhere")
ggsave("signed.png", restored, width = 6, height = 4)
options(old)
```

## A log of saved figures

The `log` option appends a row to a CSV file for every file saved,
including variants. The MD5 checksum shows whether a figure in a
manuscript still matches the one the script produced.

``` r

old <- ggsaveR_options(log = "figures-log.csv", formats = c("png", "pdf"))

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
ggsave("weight.png", p, width = 5, height = 3.5)
ggsave("weight-smooth.png", p + geom_smooth(method = "lm", formula = y ~ x),
       width = 5, height = 3.5)
options(old)

read.csv("figures-log.csv")[, c("file", "device", "width", "height", "dpi", "plot")]
#>                file device width height dpi
#> 1        weight.png    png     5    3.5 300
#> 2        weight.pdf    pdf     5    3.5  NA
#> 3 weight-smooth.png    png     5    3.5 300
#> 4 weight-smooth.pdf    pdf     5    3.5  NA
#>                                              plot
#> 1                                               p
#> 2                                               p
#> 3 p + geom_smooth(method = "lm", formula = y ~ x)
#> 4 p + geom_smooth(method = "lm", formula = y ~ x)
```
