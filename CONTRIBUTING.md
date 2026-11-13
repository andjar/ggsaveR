# Contributing to ggsaveR

Thank you for considering a contribution. Bug reports, feature requests
and pull requests are all welcome.

## Reporting a bug

Open an [issue](https://github.com/andjar/ggsaveR/issues) with a minimal
reproducible example, ideally made with the
[reprex](https://reprex.tidyverse.org/) package. Include the output of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) and the
ggsaveR options you had set
([`ggsaveR::ggsaveR_options()`](https://andjar.github.io/ggsaveR/reference/ggsaveR_options.md)
lists them).

## Proposing a change

For anything beyond a small fix, please open an issue first to discuss
the change.

1.  Fork the repository and create a branch.
2.  Make your change, and add tests in `tests/testthat/` for new
    behaviour or bug fixes.
3.  Update the roxygen comments and run `devtools::document()`.
4.  Add a bullet to the top of `NEWS.md` describing the change.
5.  Check that `devtools::check()` passes without errors, warnings or
    notes.
6.  Open a pull request.

## Design principles

- With no options set,
  [`ggsave()`](https://andjar.github.io/ggsaveR/reference/ggsave.md)
  must behave exactly like
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
- New behaviour is switched on with an option, so that existing scripts
  need no changes.
- Options are validated, and invalid values give a clear error.
- Keep hard dependencies to a minimum; optional features go in
  `Suggests`.

## Code of conduct

Please be respectful and constructive. This project follows the
[Contributor
Covenant](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
