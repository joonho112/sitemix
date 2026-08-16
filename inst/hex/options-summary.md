# sitemix Hex Sticker — 3 Design Options

**Author:** JoonHo Lee
**Date:** 2026-05-27

## How to view

Open each SVG file directly in a browser:

-   [option-A-stacked-dots.svg](option-A-stacked-dots.svg)
-   [option-B-ci-forest.svg](option-B-ci-forest.svg)
-   [option-C-typographic.svg](option-C-typographic.svg)

All three follow the locked color palette (`#1B4965` primary,
`#5FA8D3` / `#9EC5DE` / `#62B6CB` accents, `#F4F4F4` light) and
the same 618×715 hex shape.

## The three directions

### Option A — Stacked dots

**Visual:** Dark primary hex. Lowercase wordmark "sitemix"
near the top with a tiny tagline "site-year estimates". The
lower half holds a curved band of ~35 light dots in three depth
layers (translucent back, mid-band, brighter front) that
suggest many site-year estimates ranked or fanning across the
hex.

**Reads as:** *"Many sites, many estimates, one package."*

**Best for:** EB practitioners — the depth gradient suggests
shrinkage / posterior pull toward the center.

**Possible weaknesses:** At 64 px favicon size, the dot detail
collapses; the wordmark + bottom haze still reads as "sitemix".

### Option B — CI forest

**Visual:** Light hex with dark primary border. Wordmark
"sitemix" at top in primary blue, tagline "first-stage
estimates" below. The lower half is a small forest of 11
vertical confidence-interval bars (rounded caps) crossing a
horizontal dashed "common-mean" reference line, with a small
point estimate dot on each bar where it meets the line.

**Reads as:** *"Per-site estimates with explicit uncertainty,
benchmarked against a common mean."*

**Best for:** methodologists — the explicit CIs and the
reference line read as serious-statistical-graphics, the
universal forest-plot grammar that EB / meta-analysis readers
recognize instantly.

**Possible weaknesses:** Slightly busier than A or C. Favicon
size: bars compress but the silhouette still reads as
"vertical-bar plot".

### Option C — Typographic

**Visual:** Dark primary hex with a slightly lighter border.
The wordmark "sitemix" dominates the center at large size.
Below the wordmark, a small caps subtitle "SITE-YEAR · EB · R"
sets the package identity. Along the bottom edge, a
low-contrast histogram strip of 21 ticks of varying height
(bell-shaped distribution) sits above a thin baseline.

**Reads as:** *"sitemix is the package; everything else is
quietly in the background."*

**Best for:** print / sticker contexts where the wordmark
matters more than the iconography; conservative branding.

**Possible weaknesses:** Less immediately legible as a
*statistical* package than A or B (the histogram strip is
intentionally muted).

## Comparison matrix

| Attribute | Option A | Option B | Option C |
|:----------|:---------|:---------|:---------|
| Iconography primary message | Many estimates | Per-site uncertainty | Wordmark first |
| Background | Dark primary | Light | Dark primary |
| Reads at 64 px favicon | Wordmark + haze | Wordmark + bars | Wordmark dominant |
| Methods-paper figure fit | Good | Best | Good |
| Print / sticker fit | Good | Good | Best |
| Visual continuity with multisiteDGP | High | High | Medium |
| Visual complexity | Medium | Medium | Low |

## Recommendation

**Option B (CI forest)** is the canonical choice for v0.1:

1.  It reads as *statistical* at every scale.
2.  The forest plot is the universal grammar EB / meta-analysis
    readers know — instant recognition for both audiences.
3.  The light background makes the wordmark crisp on a pkgdown
    home page and reverses cleanly to dark in dark-mode browsers.

**Option A** is the second pick — the dot band is distinctive
and conveys "many site-year estimates" cleanly.

**Option C** is third — strong wordmark presence but the
iconography is the most muted of the three.

## File sizes

-   option-A-stacked-dots.svg : ~3.7 KB
-   option-B-ci-forest.svg : ~2.7 KB
-   option-C-typographic.svg : ~3.0 KB

All well under the 50 KB cap.

## Canonical source

`inst/hex/sitemix-hex.svg` holds a copy of the selected option
(currently Option B). `man/figures/logo.png` is exported at
618×715 (144 DPI) from that file, and `pkgdown/favicon/` holds
the favicon set produced from the same source.
