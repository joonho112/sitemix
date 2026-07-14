# sitemix Hex Sticker — Concept Brief

**Author:** JoonHo Lee
**Date:** 2026-05-27

## Audience

Two audiences see this sticker simultaneously:

-   **Applied researchers** (district / state accountability
    analysts, EB practitioners) scanning the pkgdown home page,
    GitHub repo card, or a methods-section figure.
-   **Methodologists / reviewers** evaluating whether the package
    is serious enough to cite in a paper.

The sticker must read as **academic and rigorous** at a glance
without looking dry, and must work both at navbar size
(~120 px tall) and at favicon size (~64 px tall).

## Tone target

| Attribute | Target | Anti-target |
|:----------|:-------|:------------|
| Mood | Calm, rigorous, academic | Playful, cartoonish |
| Density | Sparse — one strong visual idea | Busy, mosaic-like |
| Color | One primary + one optional accent | Rainbow, neon |
| Typography | Lowercase wordmark, clean sans-serif | Script, ornate, decorative |
| Iconography | Abstract gesture toward "site-level rates" | Real student photos, school logos, flags |

## Visual languages — three permitted directions

The sticker drafts produce **one SVG per direction**:

### Direction A — Stacked dots

A curved band of ~30–50 translucent dots inside the hex,
each dot suggesting one site-year estimate. Variations in
size or opacity can convey precision / shrinkage.

### Direction B — CI forest

A small forest of 10–14 vertical bars of varying height and
width inside the hex, suggesting confidence-interval ranges.
Light-gray reference line through the middle anchors a "common
mean."

### Direction C — Typographic

The wordmark `sitemix` set in a clean sans-serif inside the
hex with a subtle iconographic underline (e.g., a row of
measurement-tick marks or a low-contrast histogram strip).
Iconography stays in the lower 25 % of the hex.

## Color tokens (locked)

-   **Primary:** `#1B4965` (deep cool blue) — matches
    `multisiteDGP-public` for visual continuity with the
    maintainer's other packages.
-   **Secondary / accent (optional):** `#5FA8D3` (mid blue) for
    light dots / lines on the dark primary, or
    `#62B6CB` for the typographic variant's accent stripe.
-   **Background:** transparent outside the hex; inside, either
    solid primary or near-white (`#F4F4F4`) with primary
    foreground.

Pick **one** of two contrast modes per design:

-   *Dark-primary mode*: hex interior `#1B4965`, foreground
    `#F4F4F4` or `#5FA8D3`.
-   *Light-primary mode*: hex interior `#F4F4F4`, foreground
    `#1B4965`.

## Typography

-   Family: any clean geometric / humanist sans available in
    pure SVG (e.g., `Inter`, `Lato`, `Roboto`, `Helvetica`).
-   Weight: 500–600 for the wordmark; 300–400 for any subtitle.
-   Lowercase. Never `Sitemix` or `SITEMIX`.

## Shape constraints

-   Standard 2 : √3 hex. Width 618 px × height 715 px when
    rendered at 144 DPI for `man/figures/logo.png`.
-   1 px stroke around the hex border (in primary color) for
    sticker-print readability.

## Accessibility

-   The sticker must remain visually parseable at **64 px tall**
    (favicon size). Test by mentally squinting: can you still
    tell which direction (A/B/C) you're looking at?
-   Provide a `<title>` element inside the SVG with a short
    text description for screen readers.
-   No information conveyed solely by color hue (use
    size / position too).

## Hard prohibitions

-   No real student data, names, or institutional logos.
-   No emojis.
-   No photographic textures (PNG fills or raster embeds).
-   No proprietary fonts.

## File format and export

Each design produces:

-   `inst/hex/option-{A|B|C}-{label}.svg` — the canonical source.
-   No PNG export at the design stage; PNG export and favicon
    generation are performed downstream from the selected SVG.
-   File size cap: 50 KB per SVG.

## What success looks like

A design passes when the maintainer can answer "yes" to all
three questions on each option:

1.  Does it tell me at a glance this is about *site-level rates*?
2.  Would I be comfortable pasting this into a methods appendix?
3.  Is it legible at favicon size?
