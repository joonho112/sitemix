# Data and regression-fixture provenance

This note defines the maintainer workflow for the artifacts pinned in
`data-fixture-provenance.csv`. It distinguishes validation from regeneration.

## Simulated example panel

`prek_sim` is generated from design constants declared in
`inst/scripts/build-prek-sim.R`. The builder reads no external file, so there
is no source to obtain, validate, or protect, and any maintainer can request a
full content replay at any time:

```sh
Rscript --vanilla inst/scripts/audit-data-fixture-provenance.R \
  --out-dir=/tmp/sitemix-data-fixture \
  --replay-public-data=TRUE
```

The audit rebuilds the four public artifacts into a fresh temporary directory
and compares them against the tracked files. A `--source` argument is still
accepted for backward compatibility but is ignored.

R's serialized `.rda` and `.rds` bytes include writer-version metadata. A
replay under a different R writer can therefore be object-identical while its
file hash differs. Object/content identity is the blocking replay contract;
the currently shipped file hashes remain independently pinned. CSV and plain
text are expected to replay byte-for-byte from the recorded builder.

To validate the tracked checksums without rebuilding:

```sh
Rscript --vanilla inst/scripts/audit-data-fixture-provenance.R \
  --out-dir=/tmp/sitemix-data-fixture-public-only \
  --replay-public-data=FALSE
```

A passing checksum-only result must say that replay was not attempted and must
not make a rebuild claim.

The audit also enforces two disclosure guards: the shipped design record must
not acquire administrative-source vocabulary, and no path in the built source
package may look like a restricted or confidential input.

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
