# sitemix 0.3.1: SoftwareX reproduction materials

Author and maintainer: JoonHo Lee (jlee296@ua.edu)

Package repository: https://github.com/joonho112/sitemix

The bundled `qa/software-source` contains 178 version-0.3.1 source files
fixed by a SHA-256 manifest. This archive is self-contained with respect to
sitemix source and simulated data. It does not require a particular GitHub
commit. Check the repository's DESCRIPTION for the publicly available
version; the local archive's build does not imply that a remote tag exists.
No administrative or person-level records of real individuals are included.

## Quick reproduction

Use R/Rscript, Python 3, and a shell. Install the five runtime imports and
their dependencies (rlang, tibble, vctrs, cli, Matrix), plus jsonlite and
digest. Version records are in `qa/software-identity/reproduction-dependencies.csv`.
Run from the extracted root:

```sh
sh run-quick-checks.sh
```

The installer verifies all source hashes and installs version 0.3.1 into
`qa/r-library`. It does not download dependencies. The validators and analysis
explicitly use that library. Expected results: 263 scalar/FPC configurations,
2,371 numerical and 1,141 metadata comparisons, 174 exact census zeros and
nine expected domain errors; 118 categorical/Fréchet assertions and ten
expected errors; 126 exact-binomial grid rows and 688 checks; 38 numerical
reproduction assertions and `REPRODUCE-OK`. A session-level D1 working-
independence warning is intentional and recorded. Outputs are overwritten
when rerun; retain the downloaded ZIP as the original record.

The calculations reproduce median enrollment 23.5 for 2024 (the all-year
median is 24). Fréchet intervals condition on the supplied margins. Small-n
calculations address variance approximation and bias, not interval coverage.

## Additional verification and figures

With the listed optional packages installed:

```sh
Rscript --vanilla scripts/validate-reference-software.R
R_LIBS_USER=qa/r-library Rscript --vanilla qa/software-source/inst/scripts/audit-frechet-boundaries.R
Rscript --vanilla scripts/figures.R
```

The reference-software comparison requires survey. It checks 14 Wilson
surrogate cases against prop.test, five corrected SRSWOR scalar cases, and
one binary covariance matrix against survey. The boundary audit evaluates
3,440 integer-margin cases. Figures require ggplot2, patchwork and scales;
macOS uses quartz PDF and other systems require a functioning Cairo PDF
device. PDF bytes can vary with fonts, device, and timestamps.

The latest publication cleanup verification is recorded in
`qa/validation/publication-check/summary.json`: a fresh build/check of
the cleaned package (6,802 passing expectations with zero skips),
a separate full checkout test run (including repository checks),
documentation/data/lint gates, and a rerun of the quick reproduction.
The earlier full-test and coverage records retain their original times;
coverage was not rerun because all instrumented estimator R source
files are byte-identical.

Final local software evidence is included as JSON summaries: 6,918 passing
expectations in 511 blocks across 53 test files; standard R CMD check with
zero errors, warnings or notes; test-based line coverage 9,220/10,091
(91.3685%). These were run for 0.3.1. This archive's quick run does not rerun
the full package suite or coverage. For those, use the repository checkout
and its documented package-development workflow (including repository
metadata and DESCRIPTION Suggests). A standard R CMD build/check of the
bundled source is also possible with its build dependencies and TeX manual
toolchain. Historical 0.3.0 CI is not evidence for 0.3.1 changes.

Recorded local environment: R 4.6.0 on macOS Tahoe 26.6.2. Dependencies are
not vendored or locked. This is an isolated sitemix installation with the
recorded existing libraries, not a hermetic or cross-platform validation.

## Contents and provenance

The bundle includes analysis and figure scripts, independent validators,
final CSV/JSON outputs, dependency closure,
package source and its manifest. It omits private internal reports, failed
iteration logs, installed libraries, build trees and the system-wide package
inventory. Raw execution logs and historical coverage outputs are omitted. Absolute
paths in retained metadata use portable markers. The small-n script omits only its internal drafting-memo writer;
all calculations and CSV/JSON writers are preserved and tested. Estimator code, simulated data and numerical fixtures are unchanged.
The package snapshot follows the publication cleanup: logo drafts and
historical coverage outputs are omitted, and the fixture inventory is updated
while preserving checksum checks for every retained fixture. `archive-file-manifest.json` describes this
portable bundle; `qa/software-identity/source-files.json` describes the package.
