# sitemix Hex Sticker — Selection Record

**Selected option:** **Option B — CI forest**
**Source file:** `inst/hex/option-B-ci-forest.svg`
**Canonical source:** `inst/hex/sitemix-hex.svg` (copy of Option B)

**Selection date:** 2026-05-27
**Selected by:** JoonHo Lee

## Rationale

Per the rationale in `options-summary.md`:

1.  **Reads as statistical at every scale.** Forest plots are
    the universal grammar of EB / meta-analysis readers.
2.  **Light background** keeps the wordmark crisp on the pkgdown
    home page and the GitHub repo card.
3.  **Highest "methods-paper figure" fit** of the three options
    — a methods reviewer who sees the sticker on a paper's title
    page recognizes the statistical grammar immediately.

## Override instructions

To switch to a different option after seeing the rendered PNG on
the pkgdown site:

1.  Open `inst/hex/SELECTED.md` (this file).
2.  Change "Selected option" to "A — Stacked dots" or
    "C — Typographic" and update the "Source file" line.
3.  Copy the chosen SVG to the canonical source:
    `cp inst/hex/option-A-stacked-dots.svg inst/hex/sitemix-hex.svg`
    (or option-C), then re-render the PNG via
    `rsvg::rsvg_png()` or equivalent.
4.  Re-run `pkgdown::build_favicons()` and `pkgdown::build_home()`.

Each unselected option remains on disk in `inst/hex/` so the
swap is one file copy + one re-render.

## What gets canonicalized

After this selection:

-   `inst/hex/sitemix-hex.svg` = canonical source (copy of the
    selected option).
-   `man/figures/logo.png` = 618×715 PNG export at 144 DPI.
-   `pkgdown/favicon/` = favicon set produced via
    `pkgdown::build_favicons()`.
