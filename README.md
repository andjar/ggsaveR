# ggsaveR 🚀

<!-- badges: start -->
  [![R-CMD-check](https://github.com/andjar/ggsaveR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andjar/ggsaveR/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

**`ggsaveR` supercharges `ggplot2::ggsave()` without changing your code.** ✨

This package provides a drop-in replacement for `ggplot2::ggsave()` that adds powerful new features. The magic is that all enhancements are controlled globally via `options()`. This means you can add `library(ggsaveR)` and set a few options at the top of an existing script, and every `ggsave()` call in that script will be instantly upgraded.

## 🤔 Why `ggsaveR`?

In many data analysis projects, you might have dozens or even hundreds of `ggsave()` calls. What if you need to:

-   Save every plot as both a high-resolution `png` for a presentation and a `pdf` for a manuscript?
-   Ensure you don't accidentally overwrite a critical figure from a long-running analysis?
-   Trace a plot file back to the exact code and data that created it?

With the standard `ggplot2::ggsave()`, you would have to go back and modify every single call. `ggsaveR` solves this by letting you define these behaviors once, globally.

### `ggsaveR` 🆚 `ggplot2::ggsave`

| Feature                               | `ggplot2::ggsave`                               | `ggsaveR`                                                                                             |
| ------------------------------------- | ----------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| **Save a plot**                       | ✅ `ggsave("plot.png")`                           | ✅ `ggsave("plot.png")` (Works as a drop-in replacement)                                                |
| **Save to multiple formats**          | ❌ Requires multiple `ggsave()` calls.          | ✅ **Yes**, in one call, controlled by a global option (`ggsaveR.formats`).                           |
| **Save in multiple dimensions**       | ❌ Requires multiple `ggsave()` calls.          | ✅ **Yes**, in one call, controlled by a global option (`ggsaveR.formats`).                           |
| **Control file overwriting**          | ❌ Always overwrites existing files.            | ✅ **Yes**, choose to `"overwrite"`, `"stop"`, or create a `"unique"` filename.                       |
| **Embed reproducibility data**        | ❌ No.                                          | ✅ **Yes**, automatically embed the plot object, generating code, and session info into PNGs.         |
| **Add author metadata**               | ❌ No direct way.                               | ✅ **Yes**, set a global `ggsaveR.creator` option.                                                    |
| **Configuration Method**              | Arguments only.                                 | Arguments **and** global `options()`.                                                                 |
| **Bypass enhancements**               | N/A                                             | ✅ **Yes**, use `guard = TRUE` for a specific call to use the original `ggplot2` behavior.            |

## 📦 Installation

You can install the development version from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("andjar/ggsaveR")
```

## 🪄 The Magic: Seamless Integration with Existing Projects

This is the most powerful feature of `ggsaveR`. You don't need to find and replace every `ggsave()` call in your project.

#### Before `ggsaveR`

A typical analysis script might look like this:

```r
# --- my_analysis.R ---

library(ggplot2)

# ... lots of analysis code ...
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
ggsave("figures/mpg_vs_wt.png", p1, width = 7, height = 5)

# ... more analysis code ...
p2 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point()
ggsave("figures/sepal_vs_petal.png", p2, width = 6, height = 6)
```

#### After `ggsaveR`

To save every plot as both a `png` and a `pdf`, just add two lines at the top of your script:

```r
# --- my_analysis.R ---

library(ggplot2)
library(ggsaveR) # 1. Load the library

# 2. Set your desired global options
options(ggsaveR.formats = list(
  list(device = "png", dpi = 300),
  list(device = "pdf")
))

# ... lots of analysis code ...
p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
# NO CHANGE NEEDED HERE! This now saves mpg_vs_wt.png AND mpg_vs_wt.pdf
ggsave("figures/mpg_vs_wt.png", p1, width = 7, height = 5)

# ... more analysis code ...
p2 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point()
# NO CHANGE NEEDED HERE! This now saves sepal_vs_petal.png AND sepal_vs_petal.pdf
ggsave("figures/sepal_vs_petal.png", p2, width = 6, height = 6)
```

## 🌟 Core Features & Examples

First, let's load the libraries and create a sample plot.

```r
library(ggplot2)
library(ggsaveR)

p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
  geom_point() +
  labs(title = "Fuel Efficiency vs. Weight")
```

### 1. 🖼️ Save Multiple Formats and Dimensions

Set the `ggsaveR.formats` option to a list of plot configurations. `ggsaveR` will then use the `filename` argument as a base name and create one file for each configuration.

```r
options(ggsaveR.formats = list(
  # A high-res PNG for presentations
  list(device = "png", width = 8, height = 5, units = "in", dpi = 300),
  # A PDF for a manuscript
  list(device = "pdf", width = 18, height = 11, units = "cm"),
  # An SVG for web/editing
  list(device = "svg", width = 7, height = 4.5)
))

# This single call creates "outputs/my_plot.png", "outputs/my_plot.pdf",
# and "outputs/my_plot.svg" with the specified dimensions.
# The .png extension in the filename is ignored.
ggsave("outputs/my_plot.png", p)

# Don't forget to reset the option when you're done!
options(ggsaveR.formats = NULL)
```

### 2. 🛡️ Prevent File Overwrites

Use `ggsaveR.overwrite_action` to avoid losing work.

```r
options(ggsaveR.overwrite_action = "unique")

# Run this once: creates "safe_plot.png"
ggsave("safe_plot.png", p)

# Run this again: creates "safe_plot-1.png" instead of overwriting
ggsave("safe_plot.png", p)

# Run this a third time: creates "safe_plot-2.png"
ggsave("safe_plot.png", p)

# Reset to default
options(ggsaveR.overwrite_action = "overwrite")
```

You can also set it to `"stop"` to throw an error if the file exists.

### 3. 🧬 Reproducibility: Embed Data in PNGs

When `ggsaveR.embed_data = TRUE`, `ggsaveR` will automatically embed the `ggplot` object, the R code that generated it, and session information directly into the PNG file.

```r
options(ggsaveR.embed_data = TRUE)

# The plot object and call are captured and embedded in the file's metadata
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(color = Species)) |>
  ggsave("plot_with_data.png")

# You can then retrieve this information later
# embedded_info <- read_ggsaveR_data("plot_with_data.png")

# See the captured code
# cat(embedded_info$plot_call)
#> ggplot(iris, aes(Sepal.Length, Petal.Length)) +
#>   geom_point(aes(color = Species))

# You can even replot the original object
# embedded_info$plot_object

options(ggsaveR.embed_data = FALSE)
```

### 4. 🏃‍♂️ The Escape Hatch: `guard = TRUE`

What if you have global options set but need to bypass them for one specific plot? Use `guard = TRUE`. This tells `ggsaveR` to step aside and pass the call directly to `ggplot2::ggsave`.

```r
# Let's set a global option to create unique filenames
options(ggsaveR.overwrite_action = "unique")

# This call will create `final_figure.png`, then `final_figure-1.png`, etc.
ggsave("final_figure.png", p)

# But for this one call, we want the standard ggplot2 behavior (overwrite)
# The `guard` argument ensures ggsaveR's features are ignored for this call only.
ggsave("final_figure.png", p, guard = TRUE)
```

> **⚠️ Warning:** Using `guard = TRUE` in a script requires that the `ggsaveR` package is loaded. If it's not, the original `ggplot2::ggsave` will throw an "unused argument" error.

## ⚙️ Full List of Options

-   `ggsaveR.formats`: A `list` of lists, where each inner list defines a file to save (e.g., `list(device = "png", dpi = 300)`).
-   `ggsaveR.overwrite_action`: A character string: `"overwrite"` (default), `"unique"`, or `"stop"`.
-   `ggsaveR.embed_data`: A logical `TRUE`/`FALSE` (default `FALSE`). Controls data embedding in PNGs.
-   `ggsaveR.creator`: A character string to be used as the "Author" in file metadata (e.g., PDF properties).
