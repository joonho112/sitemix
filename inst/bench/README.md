# sitemix Performance Smoke Benchmarks

These scripts are developer gates, not package tests. They are intended to
catch large performance regressions in the main first-stage paths while
remaining portable and dependency-free. Timing, allocation profiling, and
result construction run in separate passes.

Run from the package root:

```sh
env OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  VECLIB_MAXIMUM_THREADS=1 BLAS_NUM_THREADS=1 \
  Rscript inst/bench/performance-smoke.R \
  --profile=ci-smoke \
  --warmup=1 --reps=3 --memory-reps=1 \
  --out-dir=/tmp/sitemix-performance-ci
```

Run the authoritative local closeout on the calibrated runtime:

```sh
env OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  VECLIB_MAXIMUM_THREADS=1 BLAS_NUM_THREADS=1 \
  Rscript inst/bench/performance-smoke.R \
  --profile=closeout --enforce \
  --warmup=2 --reps=5 --memory-reps=3 \
  --out-dir=/tmp/sitemix-performance-closeout
```

The CI profile always enforces crashes, schemas, dimensions, and deterministic
within-run result signatures, but records hosted-runner timing and memory as
artifacts. The exact calibrated runtime also checks pinned result signatures.
The closeout profile applies numeric hard limits only with `--enforce` (or
`SITEMIX_BENCH_ENFORCE=true`), the canonical budget and runtime hashes, an exact
R/platform/BLAS/LAPACK/dependency match, and at least 5 timing repetitions, 2
warmups, and 3 memory repetitions. Thread variables must be present before R
starts, and the output directory must not already exist.

The runtime reference records `calibration_package_version` separately from
`enforced_package_version`. The former identifies the metadata version under
which the future thresholds were first measured; the latter is the current
version required for numeric enforcement. A version retarget therefore needs
a fresh enforced closeout, while the source manifest and pinned result
signatures establish whether package behavior stayed identical.

The C and AL Pre-K thresholds are post-Phase-6 calibrations for detecting
future regressions; they are not evidence of a Phase-6 before/after gain. The
historical B-path comparison remains in `vcov-validation-profile.R` and its
frozen before/after artifacts. AL Pre-K `S=250` means 250 site-year rows (50
sites over 5 years), not 250 sites. `Rprofmem()` allocations and R heap/GC
counters are calibration or descriptive measurements only. Peak RSS is not
measured or inferred from those counters. Retained and serialized result sizes
have runtime-pinned future-regression limits; the historical B object-size
identity is the only Phase-6 before/after memory representation check.
