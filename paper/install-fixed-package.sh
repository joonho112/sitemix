#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
python3 qa/software-identity/verify-source.py
Rscript --vanilla -e 'needed <- c("rlang","tibble","vctrs","cli","Matrix","jsonlite","digest"); missing <- needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]; if(length(missing)) stop("Install prerequisites: ",paste(missing,collapse=", "))'
mkdir -p qa/r-library
R CMD INSTALL --library=qa/r-library qa/software-source
Rscript --vanilla -e '.libPaths(c(normalizePath("qa/r-library"),.libPaths())); stopifnot(packageVersion("sitemix")=="0.3.1", normalizePath(find.package("sitemix"))==normalizePath("qa/r-library/sitemix"))'
python3 qa/software-identity/verify-source.py
