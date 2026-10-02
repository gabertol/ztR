# Changelog

## ztR 0.0.0.9002

- [`use_isoplotr()`](https://gabertol.github.io/ztR/reference/use_isoplotr.md)
  no longer fails on single-row input (missing `drop = FALSE`).
- Removed `R/set_concordia_axes.R`, an old copy of
  [`add_concordia_line()`](https://gabertol.github.io/ztR/reference/add_concordia_line.md)
  that was loaded after `R/add_concordia_line.R` and silently replaced
  the fixed version (`inherit.aes = FALSE`).
- `DESCRIPTION` declares `NeedsCompilation: no`, so
  `pak::pak("gabertol/ztR")` installs on Windows without Rtools.
- Old vignettes (`zircon_geochemistry.Rmd`, garnet how-to) moved to
  `dev/`; rendered HTML removed from `vignettes/`. pkgdown needs every
  vignette listed in `_pkgdown.yml`.
- Workflows: `actions/checkout@v4`; pkgdown job gets
  `permissions: contents: write` (needed to push to `gh-pages`);
  R-CMD-check only fails on errors until the datasets are documented.
- `data/DHZ.xlsx` removed (a copy already lives in `inst/extdata`);
  stale `tidy_isoplotr.Rd` and `StatConcordia.Rd` removed by re-running
  roxygen.

## ztR 0.0.0.9001

- [`use_isoplotr()`](https://gabertol.github.io/ztR/reference/use_isoplotr.md)
  reads data as IsoplotR format 3 (see function docs for details).
