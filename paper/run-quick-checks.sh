#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
sh install-fixed-package.sh
Rscript --vanilla scripts/validate-scalar-fpc.R
Rscript --vanilla scripts/validate-categorical.R
Rscript --vanilla scripts/validate-small-n.R
Rscript --vanilla scripts/reproduce.R
python3 qa/software-identity/verify-source.py
