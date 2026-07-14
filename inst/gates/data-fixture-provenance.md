# Data and regression-fixture provenance

This note defines the maintainer workflow for the artifacts pinned in
`data-fixture-provenance.csv`. It distinguishes validation from regeneration.

## Public Alabama Pre-K sample

The restricted canonical panel is not shipped in the repository or source
package. The four public artifacts can always be validated from their recorded
checksums and mutually checked for schema and content consistency. That public
validation is **not** evidence that the artifacts were rebuilt.

When the approved restricted RDS is available locally, a maintainer may request
an isolated content replay:

```sh
Rscript --vanilla inst/scripts/audit-data-fixture-provenance.R \
  --out-dir=/tmp/sitemix-data-fixture \
  --source=/secure/path/student_panel_2021-2026.rds \
  --replay-public-data=TRUE
```

The audit checks the recorded MD5, SHA-256, dimensions, and required columns
before it launches the existing builder. Exact replay also requires the
canonical RDS's sibling CSV: the builder records that companion's 116,899-row
count and its expected 210-row difference from the 116,689-row RDS in
`build_info`. The audit therefore pins the sibling CSV's MD5 and SHA-256 when
replay is explicitly requested. All replay outputs go to a fresh temporary
directory. Neither restricted input is copied to the evidence directory.

R's serialized `.rda` and `.rds` bytes include writer-version metadata. A
replay under a different R writer can therefore be object-identical while its
file hash differs. Object/content identity is the blocking replay contract;
the currently shipped file hashes remain independently pinned. CSV and plain
text are expected to replay byte-for-byte from the approved input and builder.

Without the restricted source, run the same audit without replay:

```sh
Rscript --vanilla inst/scripts/audit-data-fixture-provenance.R \
  --out-dir=/tmp/sitemix-data-fixture-public-only \
  --replay-public-data=FALSE
```

A passing public-only result must say that source replay was not attempted and
must not make a rebuild claim. If `--source` is supplied explicitly, a missing
path or a source that disagrees with the recorded identity must fail closed.

## Regression baseline and review CSVs

`tests/testthat/_data/regression/regression-baselines.rds` is the protected
numeric baseline. The automated audit never calls
`regression_build_baselines()` and never executes
`inst/scripts/build-regression-baselines.R`. It reads the protected RDS and
calls only `regression_write_review_csvs()` into a fresh temporary directory;
the five generated review CSVs must be byte-identical to the tracked files.

Numeric baseline regeneration is a separate maintainer action requiring:

1. explicit authorization and a documented statistical reason;
2. `pkgload` loading the current source package, with source version and origin
   verified so an older installed package cannot be used as a fallback;
3. candidate output written outside the tracked fixture directory;
4. numerical, schema, and human-readable CSV review against the old baseline;
5. changelog and regression-test review; and
6. intentional replacement only after approval.

The protected builder is a maintainer repository file but is excluded from the
source package because it depends on the source test helper. Its current
working-tree handoff state must be reviewed before any eventual commit; the
audit records Git index status without staging or committing it.
