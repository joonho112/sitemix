#!/usr/bin/env Rscript

.documentation_qa_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

.documentation_qa_articles <- c(
  "a1-getting-started",
  "a2-input-formats",
  "a3-scenario-binomial",
  "a4-multivariate-multinomial",
  "a5-published-aggregates",
  "a6-diagnostics-and-suppression",
  "a7-variance-smoothing-and-frechet",
  "a8-downstream-workflows",
  "a9-case-study-alabama-prek",
  "m1-statistical-foundations",
  "m2-scalar-se-binomial",
  "m3-multivariate-sur-covariance",
  "m4-multinomial-simplex",
  "m5-aggregate-engines",
  "m6-variance-smoothing-theory",
  "m7-frechet-envelope-theory",
  "m8-output-contract"
)

.documentation_qa_reference_topics <- c(
  "alprek_subset",
  "sitemix-package",
  "sm_diagnose",
  "sm_estimate",
  "sm_estimate_from_aggregates",
  "sm_estimate_from_counts",
  "sm_frechet_envelope",
  "sm_pivot_subgroups_to_indicators",
  "sm_pivot_subgroups_to_sites",
  "sm_smooth_variance",
  "sm_suppression_report",
  "sm_vcov"
)

.documentation_qa_required_pages <- c(
  "index.html",
  "news/index.html",
  "reference/index.html",
  "articles/index.html",
  file.path("articles", paste0(.documentation_qa_articles, ".html")),
  file.path(
    "reference",
    paste0(.documentation_qa_reference_topics, ".html")
  ),
  "articles/a8-eb-handoff.html",
  "articles/m8-eb-handoff-walters-2024.html"
)

.documentation_qa_base_url <- "https://joonho112.github.io/sitemix/"
.documentation_qa_site_overlay_paths <- c(
  "_pkgdown.yml",
  "pkgdown/favicon/apple-touch-icon.png",
  "pkgdown/favicon/favicon-96x96.png",
  "pkgdown/favicon/favicon.ico",
  "pkgdown/favicon/favicon.svg",
  "pkgdown/favicon/site.webmanifest",
  "pkgdown/favicon/web-app-manifest-192x192.png",
  "pkgdown/favicon/web-app-manifest-512x512.png"
)
.documentation_qa_wordlist_sha256 <-
  "aae1a00d434fbfe63b8d302002bf299ead15b373f40c35b17a09c199b2117f36"

documentation_qa_arg_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) {
    sub(prefix, "", hit[[1L]], fixed = TRUE)
  } else {
    default
  }
}

documentation_qa_validate_args <- function(args) {
  allowed <- c("out-dir", "profile", "url-mode")
  valid <- vapply(args, function(arg) {
    any(startsWith(arg, paste0("--", allowed, "=")))
  }, logical(1))
  if (any(!valid)) {
    stop(
      "Unknown or malformed argument: ",
      args[which(!valid)[[1L]]],
      call. = FALSE
    )
  }
  observed <- sub("^--([^=]+).*$", "\\1", args)
  if (anyDuplicated(observed)) {
    stop(
      "Duplicate argument: --",
      observed[duplicated(observed)][[1L]],
      call. = FALSE
    )
  }
  if (!"out-dir" %in% observed) {
    stop("Missing required argument: --out-dir=PATH", call. = FALSE)
  }
  profile <- documentation_qa_arg_value(args, "profile", "gate")
  profiles <- c(
    "gate",
    "negative-spelling",
    "negative-link",
    "negative-generated-drift"
  )
  if (!profile %in% profiles) {
    stop("Unsupported documentation QA profile: ", profile, call. = FALSE)
  }
  url_mode <- documentation_qa_arg_value(args, "url-mode", "online")
  if (!url_mode %in% c("offline", "online")) {
    stop("`--url-mode` must be offline or online.", call. = FALSE)
  }
  invisible(TRUE)
}

documentation_qa_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    stop("The documentation QA audit must be run with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

documentation_qa_root <- function() {
  script <- documentation_qa_script_path()
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not locate the package root.", call. = FALSE)
  }
  root
}

documentation_qa_output_target <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`--out-dir` must be one nonempty path.", call. = FALSE)
  }
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  dir.create(dirname(expanded), recursive = TRUE, showWarnings = FALSE)
  target <- file.path(
    normalizePath(dirname(expanded), mustWork = TRUE),
    basename(expanded)
  )
  if (file.exists(target) || dir.exists(target)) {
    stop("Output directory must be fresh: ", target, call. = FALSE)
  }
  target
}

documentation_qa_sha256 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing documentation QA file.", call. = FALSE)
  }
  unname(tools::sha256sum(paths))
}

documentation_qa_md5 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing documentation QA file.", call. = FALSE)
  }
  unname(tools::md5sum(paths))
}

documentation_qa_copy_tree <- function(source, destination) {
  if (!dir.exists(source)) {
    stop("Cannot copy a missing directory: ", source, call. = FALSE)
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(
    source,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  if (length(entries) && !all(file.copy(
    entries,
    destination,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  ))) {
    stop("Could not copy directory tree: ", source, call. = FALSE)
  }
  invisible(destination)
}

documentation_qa_git_files <- function(root, relative) {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  result <- suppressWarnings(system2(
    "git",
    c("ls-files", "--", vapply(relative, shQuote, character(1))),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(attr(result, "status"))) {
    stop("Could not inventory Git-tracked site overlay files.", call. = FALSE)
  }
  unique(as.character(result))
}

documentation_qa_site_overlay <- function(root, package_dir) {
  pkgdown_files <- list.files(
    file.path(root, "pkgdown"),
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = FALSE
  )
  disk_relative <- c(
    if (file.exists(file.path(root, "_pkgdown.yml"))) "_pkgdown.yml",
    file.path("pkgdown", pkgdown_files)
  )
  tracked_relative <- documentation_qa_git_files(
    root,
    c("_pkgdown.yml", "pkgdown")
  )
  relative <- unique(c(
    .documentation_qa_site_overlay_paths,
    tracked_relative,
    disk_relative
  ))
  relative <- relative[order(relative, method = "radix")]
  source_paths <- file.path(root, relative)
  source_exists <- file.exists(source_paths) & !dir.exists(source_paths)
  source_md5 <- source_sha256 <- rep(NA_character_, length(relative))
  source_md5[source_exists] <- documentation_qa_md5(source_paths[source_exists])
  source_sha256[source_exists] <- documentation_qa_sha256(
    source_paths[source_exists]
  )
  tracked <- relative %in% tracked_relative
  expected <- relative %in% .documentation_qa_site_overlay_paths

  if (!file.copy(
    file.path(root, "_pkgdown.yml"),
    file.path(package_dir, "_pkgdown.yml")
  )) {
    stop("Could not overlay `_pkgdown.yml` on the source package.", call. = FALSE)
  }
  documentation_qa_copy_tree(
    file.path(root, "pkgdown"),
    file.path(package_dir, "pkgdown")
  )

  destination_paths <- file.path(package_dir, relative)
  destination_exists <- file.exists(destination_paths) &
    !dir.exists(destination_paths)
  destination_md5 <- destination_sha256 <- rep(
    NA_character_,
    length(relative)
  )
  destination_md5[destination_exists] <- documentation_qa_md5(
    destination_paths[destination_exists]
  )
  destination_sha256[destination_exists] <- documentation_qa_sha256(
    destination_paths[destination_exists]
  )
  data.frame(
    path = relative,
    expected_path = expected,
    git_tracked = tracked,
    source_exists = source_exists,
    source_md5 = source_md5,
    source_sha256 = source_sha256,
    destination_exists = destination_exists,
    destination_md5 = destination_md5,
    destination_sha256 = destination_sha256,
    exact = source_exists & destination_exists &
      source_md5 == destination_md5 &
      source_sha256 == destination_sha256,
    stringsAsFactors = FALSE
  )
}

documentation_qa_require_packages <- function(root) {
  packages <- c(
    "curl", "knitr", "pkgdown", "pkgload", "rmarkdown", "roxygen2",
    "spelling", "urlchecker", "xml2", "yaml"
  )
  versions <- vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Documentation QA requires package `", package, "`.", call. = FALSE)
    }
    as.character(utils::packageVersion(package))
  }, character(1))
  configured <- unname(read.dcf(file.path(root, "DESCRIPTION"))[
    1L,
    "Config/roxygen2/version"
  ])
  if (!identical(versions[["roxygen2"]], configured)) {
    stop(
      "Installed roxygen2 ", versions[["roxygen2"]],
      " disagrees with configured version ", configured, ".",
      call. = FALSE
    )
  }
  data.frame(
    component = c(
      packages,
      "R", "platform", "Pandoc", "Quarto", "configured_roxygen2"
    ),
    version = c(
      unname(versions),
      as.character(getRversion()),
      R.version$platform,
      as.character(rmarkdown::pandoc_version()),
      tryCatch(
        as.character(quarto::quarto_version()),
        error = function(error) "not_available"
      ),
      configured
    ),
    stringsAsFactors = FALSE
  )
}

documentation_qa_protected_manifest <- function(root) {
  paths <- file.path(root, names(.documentation_qa_protected_sha256))
  present <- file.exists(paths)
  observed <- rep(NA_character_, length(paths))
  observed[present] <- documentation_qa_sha256(paths[present])
  data.frame(
    path = names(.documentation_qa_protected_sha256),
    expected_sha256 = unname(.documentation_qa_protected_sha256),
    observed_sha256 = observed,
    exact = present & observed == unname(.documentation_qa_protected_sha256),
    stringsAsFactors = FALSE
  )
}

documentation_qa_build_source <- function(root, work_dir) {
  build_dir <- file.path(work_dir, "source-build")
  extract_dir <- file.path(work_dir, "source-extract")
  log_path <- file.path(work_dir, "source-build.log")
  dir.create(build_dir, recursive = TRUE)
  dir.create(extract_dir, recursive = TRUE)
  old <- setwd(build_dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "build", "--no-build-vignettes", "--no-manual",
      shQuote(root)
    ),
    stdout = log_path,
    stderr = log_path
  )
  tarballs <- list.files(build_dir, pattern = "[.]tar[.]gz$", full.names = TRUE)
  if (!identical(as.integer(status), 0L) || length(tarballs) != 1L) {
    stop("Documentation QA source build failed; see source-build.log.", call. = FALSE)
  }
  utils::untar(tarballs[[1L]], exdir = extract_dir)
  packages <- list.dirs(extract_dir, recursive = FALSE, full.names = TRUE)
  packages <- packages[file.exists(file.path(packages, "DESCRIPTION"))]
  if (length(packages) != 1L) {
    stop("Could not identify the extracted source package.", call. = FALSE)
  }
  package_dir <- normalizePath(packages[[1L]], mustWork = TRUE)
  site_overlay <- documentation_qa_site_overlay(root, package_dir)
  vignette_files <- list.files(
    file.path(package_dir, "vignettes"),
    pattern = "^[am][0-9].*[.]Rmd$",
    full.names = FALSE
  )
  vignette_ids <- sub("-.*$", "", vignette_files)
  expected_ids <- c(paste0("a", 1:9), paste0("m", 1:8))
  if (!identical(sort(vignette_ids), sort(expected_ids))) {
    stop("Source package does not contain the canonical 17 vignettes.", call. = FALSE)
  }
  list(
    package_dir = package_dir,
    tarball = tarballs[[1L]],
    tarball_sha256 = documentation_qa_sha256(tarballs[[1L]]),
    build_log = log_path,
    canonical_vignettes = length(vignette_files),
    site_overlay = site_overlay
  )
}

documentation_qa_wordlist <- function(package_dir) {
  path <- file.path(package_dir, "inst", "WORDLIST")
  entries <- readLines(path, warn = FALSE)
  contract <- data.frame(
    entries = length(entries),
    empty_entries = sum(!nzchar(entries)),
    duplicate_entries = anyDuplicated(entries),
    expected_sha256 = .documentation_qa_wordlist_sha256,
    observed_sha256 = documentation_qa_sha256(path),
    reviewed_sha256_exact = identical(
      documentation_qa_sha256(path),
      .documentation_qa_wordlist_sha256
    ),
    stale_consumer_terms = sum(entries %in% c("ebrecipe", "as_eb_input")),
    stringsAsFactors = FALSE
  )
  list(
    contract = contract,
    entries = data.frame(
      index = seq_along(entries),
      entry = entries,
      stringsAsFactors = FALSE
    )
  )
}

documentation_qa_spelling <- function(package_dir) {
  result <- spelling::spell_check_package(
    package_dir,
    vignettes = TRUE,
    use_wordlist = TRUE
  )
  if (!nrow(result)) {
    return(data.frame(
      word = character(),
      found = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    word = as.character(result$word),
    found = vapply(result$found, paste, collapse = "|", character(1)),
    stringsAsFactors = FALSE
  )
}

documentation_qa_authored_url_rows <- function(package_dir) {
  config_path <- file.path(package_dir, "_pkgdown.yml")
  config <- yaml::read_yaml(config_path)
  config_values <- unname(unlist(config, recursive = TRUE, use.names = FALSE))
  config_urls <- as.character(config_values[grepl(
    "^https?://",
    config_values
  )])

  bib_path <- file.path(package_dir, "vignettes", "references.bib")
  bib <- trimws(readLines(bib_path, warn = FALSE))
  field <- function(name) {
    rows <- grep(
      paste0("^", name, "[[:space:]]*="),
      bib,
      value = TRUE,
      ignore.case = TRUE
    )
    value <- sub("^[^=]+=[[:space:]]*[{\"]", "", rows)
    sub("[}\"][[:space:]]*,?[[:space:]]*$", "", value)
  }
  bib_urls <- field("url")
  bib_dois <- field("doi")
  bib_doi_urls <- paste0("https://doi.org/", bib_dois[nzchar(bib_dois)])

  data.frame(
    URL = c(config_urls, bib_urls, bib_doi_urls),
    Parent = c(
      rep("_pkgdown.yml", length(config_urls)),
      rep("vignettes/references.bib", length(bib_urls) + length(bib_doi_urls))
    ),
    stringsAsFactors = FALSE
  )
}

documentation_qa_url_inventory <- function(package_dir) {
  source_db <- get(
    "url_db_from_package_sources",
    envir = asNamespace("tools")
  )(normalizePath(package_dir))
  vignette_db <- get(
    "url_db_from_package_rmd_vignettes",
    envir = asNamespace("urlchecker")
  )(normalizePath(package_dir))
  authored_db <- documentation_qa_authored_url_rows(package_dir)
  database <- rbind(source_db, vignette_db, authored_db)
  class(database) <- c("url_db", "data.frame")
  urls <- as.character(database$URL)
  parents <- as.character(database$Parent)
  scheme <- ifelse(
    grepl("^[[:alpha:]][[:alnum:]+.-]*:", urls),
    tolower(sub(":.*$", "", urls)),
    "relative"
  )
  inventory <- data.frame(
    occurrence = seq_along(urls),
    url = urls,
    parent = parents,
    scheme = scheme,
    https = startsWith(urls, "https://"),
    stringsAsFactors = FALSE
  )
  offline_issue <- !nzchar(urls) |
    grepl("[[:space:]]", urls) |
    (!scheme %in% c("relative", "http", "https", "mailto"))
  offline <- inventory[offline_issue, , drop = FALSE]
  if (nrow(offline)) {
    offline$issue <- "malformed_or_unsupported_uri"
  } else {
    offline <- data.frame(
      occurrence = integer(),
      url = character(),
      parent = character(),
      scheme = character(),
      https = logical(),
      issue = character(),
      stringsAsFactors = FALSE
    )
  }
  list(database = database, inventory = inventory, offline_issues = offline)
}

documentation_qa_flatten_url_issues <- function(result) {
  if (!nrow(result)) {
    return(data.frame(
      url = character(),
      from = character(),
      status = character(),
      message = character(),
      replacement = character(),
      cran = character(),
      spaces = character(),
      r = character(),
      stringsAsFactors = FALSE
    ))
  }
  frame <- as.data.frame(result, stringsAsFactors = FALSE)
  frame[] <- lapply(frame, function(column) {
    if (is.list(column)) {
      vapply(column, paste, collapse = "|", character(1))
    } else {
      as.character(column)
    }
  })
  names(frame) <- tolower(names(frame))
  names(frame)[names(frame) == "new"] <- "replacement"
  expected <- c(
    "url", "from", "status", "message", "replacement",
    "cran", "spaces", "r"
  )
  for (name in setdiff(expected, names(frame))) {
    frame[[name]] <- ""
  }
  frame[expected]
}

documentation_qa_doi_result <- function(url) {
  handle <- curl::new_handle(
    nobody = TRUE,
    followlocation = FALSE,
    useragent = paste0(
      "sitemix documentation QA (+",
      .documentation_qa_base_url,
      ")"
    )
  )
  response <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = identity
  )
  if (inherits(response, "error")) {
    return(data.frame(
      url = url,
      http_status = NA_integer_,
      resolved_url = "",
      pass = FALSE,
      message = conditionMessage(response),
      stringsAsFactors = FALSE
    ))
  }
  headers <- curl::parse_headers_list(response$headers)
  resolved <- headers[["location"]]
  if (is.null(resolved)) {
    resolved <- ""
  }
  redirect <- response$status_code >= 300L && response$status_code < 400L
  pass <- response$status_code >= 200L && response$status_code < 400L &&
    (!redirect || nzchar(resolved))
  data.frame(
    url = url,
    http_status = response$status_code,
    resolved_url = resolved,
    pass = pass,
    message = if (pass) "" else "DOI resolver did not return a valid response.",
    stringsAsFactors = FALSE
  )
}

documentation_qa_online_urls <- function(package_dir, database, url_mode) {
  urls <- unique(as.character(database$URL))
  urls <- urls[grepl("^https?://", urls)]
  targets <- data.frame(
    url = urls,
    scheme = tolower(sub(":.*$", "", urls)),
    method = ifelse(
      startsWith(tolower(urls), "https://doi.org/"),
      "doi_resolver_no_follow",
      "urlchecker"
    ),
    checked = FALSE,
    pass = FALSE,
    http_status = NA_integer_,
    resolved_url = "",
    message = "",
    result_semantics = "not_requested",
    stringsAsFactors = FALSE
  )
  if (identical(url_mode, "offline")) {
    return(list(
      status = "NOT_REQUESTED",
      issues = documentation_qa_flatten_url_issues(data.frame()),
      targets = targets,
      log = "Online URL checking was not requested."
    ))
  }
  old <- setwd(package_dir)
  on.exit(setwd(old), add = TRUE)
  doi_row <- startsWith(tolower(as.character(database$URL)), "https://doi.org/")
  regular_database <- database[!doi_row, , drop = FALSE]
  class(regular_database) <- c("url_db", "data.frame")
  output <- capture.output(
    result <- urlchecker::url_check(
      path = ".",
      db = regular_database,
      parallel = FALSE,
      progress = FALSE
    )
  )
  issues <- documentation_qa_flatten_url_issues(result)
  regular_target <- targets$method == "urlchecker"
  targets$checked[regular_target] <- TRUE
  targets$pass[regular_target] <- !targets$url[regular_target] %in% issues$url
  targets$result_semantics[regular_target] <- "urlchecker_no_issue_returned"
  for (index in which(regular_target & !targets$pass)) {
    issue <- issues[issues$url == targets$url[[index]], , drop = FALSE]
    if (nrow(issue)) {
      targets$http_status[[index]] <- suppressWarnings(
        as.integer(issue$status[[1L]])
      )
      targets$message[[index]] <- issue$message[[1L]]
    }
  }

  doi_targets <- which(targets$method == "doi_resolver_no_follow")
  doi_results <- lapply(targets$url[doi_targets], documentation_qa_doi_result)
  if (length(doi_results)) {
    doi_results <- do.call(rbind, doi_results)
    targets$checked[doi_targets] <- TRUE
    targets$pass[doi_targets] <- doi_results$pass
    targets$http_status[doi_targets] <- doi_results$http_status
    targets$resolved_url[doi_targets] <- doi_results$resolved_url
    targets$message[doi_targets] <- doi_results$message
    targets$result_semantics[doi_targets] <-
      "doi_resolver_valid_response_without_publisher_follow"
    failed_doi <- doi_results[!doi_results$pass, , drop = FALSE]
    if (nrow(failed_doi)) {
      parent <- vapply(failed_doi$url, function(url) {
        as.character(database$Parent[match(url, database$URL)])
      }, character(1))
      doi_issues <- data.frame(
        url = failed_doi$url,
        from = parent,
        status = as.character(failed_doi$http_status),
        message = failed_doi$message,
        replacement = failed_doi$resolved_url,
        cran = "",
        spaces = "",
        r = "",
        stringsAsFactors = FALSE
      )
      issues <- rbind(issues, doi_issues)
    }
  }
  list(
    status = if (nrow(issues) || any(!targets$pass)) "FAIL" else "PASS",
    issues = issues,
    targets = targets,
    log = paste(
      c(
        paste0(
          "URL mode: online; targets: ", nrow(targets),
          "; DOI targets: ", length(doi_targets),
          "; issue rows: ", nrow(issues)
        ),
        output
      ),
      collapse = "\n"
    )
  )
}

documentation_qa_rd_diagnostic_rows <- function(database) {
  rows <- lapply(names(database), function(name) {
    checked <- tools::checkRd(database[[name]])
    if (!length(checked)) {
      return(NULL)
    }
    diagnostics <- capture.output(print(checked))
    data.frame(
      file = name,
      check = "syntax",
      diagnostic = diagnostics,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      file = character(),
      check = character(),
      diagnostic = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

documentation_qa_rd_content_rows <- function(package_dir) {
  checked <- tools::checkRdContents(dir = package_dir)
  if (!length(checked)) {
    return(data.frame(
      file = character(),
      check = character(),
      diagnostic = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(names(checked), function(name) {
    data.frame(
      file = name,
      check = "content",
      diagnostic = paste(capture.output(str(checked[[name]])), collapse = " "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

documentation_qa_rd_xref_inventory <- function(package_dir) {
  database <- get(
    ".build_Rd_xref_db",
    envir = asNamespace("tools")
  )(dir = package_dir)
  rows <- lapply(names(database), function(name) {
    value <- database[[name]]
    if (!NROW(value)) {
      return(NULL)
    }
    data.frame(
      file = name,
      target = as.character(value[, 1L]),
      anchor = as.character(value[, 2L]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      file = character(),
      target = character(),
      anchor = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

documentation_qa_rd_xref_issues <- function(package_dir) {
  variables <- c(
    "_R_CHECK_XREFS_PKGS_ARE_DECLARED_",
    "_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_"
  )
  prior <- Sys.getenv(variables, unset = NA_character_)
  on.exit({
    for (index in seq_along(variables)) {
      if (is.na(prior[[index]])) {
        Sys.unsetenv(variables[[index]])
      } else {
        do.call(Sys.setenv, setNames(list(prior[[index]]), variables[[index]]))
      }
    }
  }, add = TRUE)
  Sys.setenv(
    `_R_CHECK_XREFS_PKGS_ARE_DECLARED_` = "true",
    `_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_` = "true"
  )
  checked <- get(
    ".check_Rd_xrefs",
    envir = asNamespace("tools")
  )(dir = package_dir)
  rows <- list()
  for (category in names(checked)) {
    value <- checked[[category]]
    if (!length(value)) {
      next
    }
    if (is.list(value)) {
      for (file in names(value)) {
        if (length(value[[file]])) {
          rows[[length(rows) + 1L]] <- data.frame(
            category = category,
            file = file,
            reference = as.character(value[[file]]),
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        category = category,
        file = "package",
        reference = as.character(value),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      category = character(),
      file = character(),
      reference = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

documentation_qa_rd <- function(package_dir) {
  database <- tools::Rd_db(dir = package_dir)
  syntax <- documentation_qa_rd_diagnostic_rows(database)
  content <- documentation_qa_rd_content_rows(package_dir)
  list(
    files = names(database),
    diagnostics = rbind(syntax, content),
    xrefs = documentation_qa_rd_xref_inventory(package_dir),
    xref_issues = documentation_qa_rd_xref_issues(package_dir)
  )
}

documentation_qa_pkgdown <- function(package_dir, log_path) {
  warnings <- character()
  messages <- character()
  errors <- character()
  site_dir <- file.path(package_dir, "docs")
  if (dir.exists(site_dir) || file.exists(site_dir)) {
    stop("Extracted source package unexpectedly contains `docs/`.", call. = FALSE)
  }
  output <- capture.output(
    withCallingHandlers(
      tryCatch(
        {
          old <- setwd(package_dir)
          on.exit(setwd(old), add = TRUE)
          RNGkind("Mersenne-Twister", "Inversion", "Rejection")
          set.seed(1L)
          pkgdown::check_pkgdown(pkg = ".")
          pkgdown::build_site(
            pkg = ".",
            examples = FALSE,
            seed = 1L,
            preview = FALSE,
            new_process = FALSE,
            install = TRUE,
            quiet = TRUE
          )
        },
        error = function(error) {
          errors <<- c(errors, conditionMessage(error))
          NULL
        }
      ),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      },
      message = function(message) {
        messages <<- c(messages, conditionMessage(message))
        invokeRestart("muffleMessage")
      }
    ),
    type = "output"
  )
  writeLines(
    c(
      output,
      paste0("MESSAGE: ", messages),
      paste0("WARNING: ", warnings),
      paste0("ERROR: ", errors)
    ),
    log_path,
    useBytes = TRUE
  )
  list(
    site_dir = site_dir,
    warnings = warnings,
    messages = messages,
    errors = errors,
    pass = !length(warnings) && !length(errors) && dir.exists(site_dir)
  )
}

documentation_qa_site_target <- function(site_dir, source, url) {
  local <- url
  root_relative <- FALSE
  if (startsWith(local, .documentation_qa_base_url)) {
    local <- substring(local, nchar(.documentation_qa_base_url) + 1L)
    root_relative <- TRUE
  } else if (grepl("^https?://", local)) {
    return(list(kind = "external", target = NA_character_, fragment = ""))
  } else if (startsWith(local, "//")) {
    return(list(kind = "external", target = NA_character_, fragment = ""))
  } else if (grepl("^[[:alpha:]][[:alnum:]+.-]*:", local)) {
    return(list(kind = "scheme", target = NA_character_, fragment = ""))
  }
  fragment <- if (grepl("#", local, fixed = TRUE)) {
    sub("^[^#]*#", "", local)
  } else {
    ""
  }
  path <- sub("[?#].*$", "", local)
  path <- utils::URLdecode(path)
  if (startsWith(path, "/sitemix/")) {
    path <- substring(path, nchar("/sitemix/") + 1L)
    root_relative <- TRUE
  } else if (startsWith(path, "/")) {
    path <- substring(path, 2L)
    root_relative <- TRUE
  }
  if (!nzchar(path)) {
    relative <- source
  } else if (root_relative) {
    relative <- path
  } else {
    relative <- file.path(dirname(source), path)
  }
  root <- normalizePath(site_dir, mustWork = TRUE)
  target <- normalizePath(file.path(root, relative), mustWork = FALSE)
  inside <- identical(target, root) ||
    startsWith(target, paste0(root, .Platform$file.sep))
  if (!inside) {
    return(list(kind = "outside", target = target, fragment = fragment))
  }
  if (dir.exists(target)) {
    target <- file.path(target, "index.html")
  }
  list(kind = "local", target = target, fragment = fragment)
}

documentation_qa_anchor_exists <- function(path, fragment, cache) {
  if (!nzchar(fragment) || !file.exists(path) || !grepl("[.]html$", path)) {
    return(NA)
  }
  key <- path
  if (!exists(key, envir = cache, inherits = FALSE)) {
    document <- xml2::read_html(path, encoding = "UTF-8")
    ids <- xml2::xml_attr(xml2::xml_find_all(document, "//*[@id]"), "id")
    names <- xml2::xml_attr(
      xml2::xml_find_all(document, "//a[@name]"),
      "name"
    )
    assign(key, unique(c(ids, names)), envir = cache)
  }
  decoded <- utils::URLdecode(fragment)
  decoded %in% get(key, envir = cache, inherits = FALSE)
}

documentation_qa_site_links <- function(site_dir) {
  html <- sort(list.files(
    site_dir,
    pattern = "[.]html$",
    recursive = TRUE,
    full.names = TRUE
  ))
  root_length <- nchar(normalizePath(site_dir)) + 2L
  cache <- new.env(parent = emptyenv())
  rows <- list()
  selectors <- list(
    c("a", "href"),
    c("img", "src"),
    c("script", "src"),
    c("link", "href"),
    c("source", "src")
  )
  for (path in html) {
    source <- substring(normalizePath(path), root_length)
    document <- xml2::read_html(path, encoding = "UTF-8")
    for (selector in selectors) {
      nodes <- xml2::xml_find_all(
        document,
        paste0("//", selector[[1L]], "[@", selector[[2L]], "]")
      )
      urls <- xml2::xml_attr(nodes, selector[[2L]])
      for (url in urls[!is.na(urls) & nzchar(urls)]) {
        resolved <- documentation_qa_site_target(site_dir, source, url)
        target_exists <- if (identical(resolved$kind, "local")) {
          file.exists(resolved$target)
        } else {
          NA
        }
        anchor_exists <- if (identical(resolved$kind, "local")) {
          documentation_qa_anchor_exists(
            resolved$target,
            resolved$fragment,
            cache
          )
        } else {
          NA
        }
        pass <- if (identical(resolved$kind, "local")) {
          isTRUE(target_exists) && !identical(anchor_exists, FALSE)
        } else {
          !identical(resolved$kind, "outside")
        }
        target_display <- if (identical(resolved$kind, "local")) {
          substring(
            normalizePath(resolved$target, mustWork = FALSE),
            nchar(normalizePath(site_dir, mustWork = TRUE)) + 2L
          )
        } else if (is.na(resolved$target)) {
          ""
        } else {
          resolved$target
        }
        rows[[length(rows) + 1L]] <- data.frame(
          source = source,
          element = selector[[1L]],
          attribute = selector[[2L]],
          url = url,
          link_kind = resolved$kind,
          target = target_display,
          fragment = resolved$fragment,
          target_exists = target_exists,
          anchor_exists = anchor_exists,
          pass = pass,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      source = character(),
      element = character(),
      attribute = character(),
      url = character(),
      link_kind = character(),
      target = character(),
      fragment = character(),
      target_exists = logical(),
      anchor_exists = logical(),
      pass = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

documentation_qa_required_pages <- function(site_dir) {
  paths <- file.path(site_dir, .documentation_qa_required_pages)
  exists <- file.exists(paths)
  size <- rep(NA_real_, length(paths))
  sha256 <- rep(NA_character_, length(paths))
  size[exists] <- file.info(paths[exists])$size
  sha256[exists] <- documentation_qa_sha256(paths[exists])
  data.frame(
    path = .documentation_qa_required_pages,
    exists = exists,
    size_bytes = size,
    sha256 = sha256,
    stringsAsFactors = FALSE
  )
}

documentation_qa_run_child <- function(root, script_name, out_dir, log_path) {
  script <- file.path("inst", "scripts", script_name)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", script, paste0("--out-dir=", out_dir)),
    stdout = log_path,
    stderr = log_path
  ))
  list(status = as.integer(status), output = out_dir, log = log_path)
}

documentation_qa_self_tests <- function(work_dir) {
  spelling_path <- file.path(work_dir, "negative-spelling.Rmd")
  writeLines(
    "This fixture contains documantationqaz.",
    spelling_path,
    useBytes = TRUE
  )
  spelling_result <- spelling::spell_check_files(spelling_path, lang = "en_US")
  spelling_detected <- nrow(spelling_result) > 0L &&
    "documantationqaz" %in% spelling_result$word

  link_site <- file.path(work_dir, "negative-link-site")
  dir.create(link_site)
  writeLines(
    '<html><body><a href="missing.html#absent">broken</a></body></html>',
    file.path(link_site, "index.html"),
    useBytes = TRUE
  )
  link_result <- documentation_qa_site_links(link_site)
  link_detected <- nrow(link_result) > 0L && any(!link_result$pass)

  current <- file.path(work_dir, "negative-current.txt")
  regenerated <- file.path(work_dir, "negative-regenerated.txt")
  writeLines("current generated documentation", current, useBytes = TRUE)
  writeLines("drifted generated documentation", regenerated, useBytes = TRUE)
  drift_detected <- !identical(
    documentation_qa_sha256(current),
    documentation_qa_sha256(regenerated)
  )

  data.frame(
    fixture = c("bad_spelling", "broken_link", "generated_drift"),
    expected_detector_failures = c(1L, 1L, 1L),
    observed_detector_failures = c(
      as.integer(spelling_detected),
      sum(!link_result$pass),
      as.integer(drift_detected)
    ),
    detected = c(spelling_detected, link_detected, drift_detected),
    repository_modified = FALSE,
    stringsAsFactors = FALSE
  )
}

documentation_qa_budget <- function(root, observed) {
  path <- file.path(root, "inst", "gates", "documentation-qa-budget.csv")
  budget <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  expected <- c("metric", "comparison", "threshold", "rationale")
  if (!identical(names(budget), expected) || nrow(budget) != 30L) {
    stop("Documentation QA budget schema or row count drifted.", call. = FALSE)
  }
  if (anyDuplicated(budget$metric) || any(!nzchar(budget$rationale))) {
    stop("Documentation QA budget contains duplicate or empty fields.", call. = FALSE)
  }
  if (!setequal(unique(budget$comparison), c("at_least", "at_most", "exact"))) {
    stop("Documentation QA budget comparison vocabulary drifted.", call. = FALSE)
  }
  if (!setequal(budget$metric, names(observed))) {
    stop("Documentation QA observations disagree with the budget metrics.", call. = FALSE)
  }
  budget$observed <- as.numeric(observed[budget$metric])
  budget$pass <- mapply(
    function(comparison, actual, threshold) {
      switch(
        comparison,
        at_least = actual >= threshold,
        at_most = actual <= threshold,
        exact = actual == threshold,
        FALSE
      )
    },
    budget$comparison,
    budget$observed,
    budget$threshold,
    USE.NAMES = FALSE
  )
  budget
}

documentation_qa_component_row <- function(component, pass, detail) {
  data.frame(
    component = component,
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

documentation_qa_source_manifest <- function(root) {
  relative <- c(
    "DESCRIPTION",
    ".Rbuildignore",
    "_pkgdown.yml",
    "README.Rmd",
    "README.md",
    "NEWS.md",
    "R/sitemix-package.R",
    "man/sitemix-package.Rd",
    "inst/WORDLIST",
    "inst/gates/documentation-qa-budget.csv",
    "inst/scripts/audit-documentation-qa.R",
    "inst/scripts/audit-documentation-drift.R",
    "inst/scripts/audit-vignette-reproducibility.R",
    "tests/testthat/test-test-architecture.R",
    "vignettes/a8-downstream-workflows.Rmd",
    "vignettes/m8-output-contract.Rmd",
    "vignettes/references.bib",
    names(.documentation_qa_protected_sha256)
  )
  paths <- file.path(root, relative)
  data.frame(
    path = relative,
    size_bytes = file.info(paths)$size,
    md5 = documentation_qa_md5(paths),
    sha256 = documentation_qa_sha256(paths),
    stringsAsFactors = FALSE
  )
}

documentation_qa_write_outputs <- function(
  target,
  summary,
  components,
  budget,
  spelling,
  wordlist,
  url_inventory,
  url_offline,
  url_online,
  url_online_targets,
  rd_diagnostics,
  rd_xrefs,
  rd_xref_issues,
  pages,
  links,
  coupling,
  self_tests,
  protected,
  toolchain,
  source_manifest,
  built,
  pkgdown_result,
  doc_child,
  vignette_child,
  work_dir
) {
  staging <- paste0(target, ".tmp-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging)) {
    stop("Documentation QA staging directory already exists.", call. = FALSE)
  }
  dir.create(staging)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  tables <- list(
    "documentation-qa-summary.csv" = summary,
    "documentation-qa-components.csv" = components,
    "documentation-qa-budget-results.csv" = budget,
    "documentation-spelling.csv" = spelling,
    "documentation-wordlist-contract.csv" = wordlist$contract,
    "documentation-wordlist-entries.csv" = wordlist$entries,
    "documentation-url-inventory.csv" = url_inventory,
    "documentation-url-offline-issues.csv" = url_offline,
    "documentation-url-online-issues.csv" = url_online,
    "documentation-url-online-targets.csv" = url_online_targets,
    "documentation-rd-diagnostics.csv" = rd_diagnostics,
    "documentation-rd-xrefs.csv" = rd_xrefs,
    "documentation-rd-xref-issues.csv" = rd_xref_issues,
    "documentation-pkgdown-pages.csv" = pages,
    "documentation-pkgdown-links.csv" = links,
    "documentation-current-coupling.csv" = coupling,
    "documentation-negative-self-tests.csv" = self_tests,
    "documentation-protected.csv" = protected,
    "documentation-toolchain.csv" = toolchain,
    "documentation-source-manifest.csv" = source_manifest,
    "documentation-site-overlay-manifest.csv" = built$site_overlay
  )
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(staging, name), row.names = FALSE)
  }
  logs <- c(
    built$build_log,
    file.path(work_dir, "pkgdown.log"),
    file.path(work_dir, "url-online.log"),
    doc_child$log,
    vignette_child$log
  )
  log_names <- c(
    "source-build.log",
    "pkgdown.log",
    "url-online.log",
    "documentation-drift-child.log",
    "vignette-child.log"
  )
  if (!all(file.copy(logs, file.path(staging, log_names)))) {
    stop("Could not copy documentation QA execution logs.", call. = FALSE)
  }
  if (!file.copy(built$tarball, file.path(staging, "source-package.tar.gz"))) {
    stop("Could not preserve the validated source tarball.", call. = FALSE)
  }
  documentation_qa_copy_tree(
    pkgdown_result$site_dir,
    file.path(staging, "pkgdown-site")
  )
  documentation_qa_copy_tree(
    doc_child$output,
    file.path(staging, "documentation-drift-evidence")
  )
  documentation_qa_copy_tree(
    vignette_child$output,
    file.path(staging, "vignette-evidence")
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "session-info.txt"),
    useBytes = TRUE
  )
  saveRDS(
    list(
      summary = summary,
      components = components,
      budget = budget,
      spelling = spelling,
      wordlist = wordlist,
      url_inventory = url_inventory,
      url_offline_issues = url_offline,
      url_online_issues = url_online,
      url_online_targets = url_online_targets,
      rd_diagnostics = rd_diagnostics,
      rd_xrefs = rd_xrefs,
      rd_xref_issues = rd_xref_issues,
      pages = pages,
      links = links,
      coupling = coupling,
      self_tests = self_tests,
      protected = protected,
      toolchain = toolchain,
      source_manifest = source_manifest,
      site_overlay = built$site_overlay,
      source_tarball_sha256 = built$tarball_sha256,
      child_status = c(
        documentation_drift = doc_child$status,
        vignette = vignette_child$status
      )
    ),
    file.path(staging, "documentation-qa-evidence.rds"),
    version = 3
  )
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit documentation QA artifacts.", call. = FALSE)
  }
  committed <- TRUE
  normalizePath(target, mustWork = TRUE)
}

documentation_qa_write_negative <- function(
  target,
  profile,
  selected,
  self_tests,
  protected,
  toolchain
) {
  staging <- paste0(target, ".tmp-", Sys.getpid())
  dir.create(staging)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  detected <- isTRUE(selected$detected)
  summary <- data.frame(
    status = if (detected) "FAIL" else "SELF_TEST_MISSED",
    profile = profile,
    expected_failure_category = selected$fixture,
    observed_detector_failures = selected$observed_detector_failures,
    expected_failure_detected = detected,
    protected_failures = sum(
      !protected$exact | !protected$unchanged_during_gate
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    summary,
    file.path(staging, "documentation-qa-summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    self_tests,
    file.path(staging, "documentation-negative-self-tests.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    protected,
    file.path(staging, "documentation-protected.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    toolchain,
    file.path(staging, "documentation-toolchain.csv"),
    row.names = FALSE
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "session-info.txt"),
    useBytes = TRUE
  )
  saveRDS(
    list(
      summary = summary,
      self_tests = self_tests,
      protected = protected,
      toolchain = toolchain
    ),
    file.path(staging, "documentation-qa-evidence.rds"),
    version = 3
  )
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit negative QA artifacts.", call. = FALSE)
  }
  committed <- TRUE
  list(summary = summary, detected = detected)
}

documentation_qa_negative_main <- function(
  root,
  target,
  profile,
  toolchain
) {
  protected_before <- documentation_qa_protected_manifest(root)
  work_dir <- tempfile("sitemix-documentation-qa-negative-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  self_tests <- documentation_qa_self_tests(work_dir)
  fixture <- sub("^negative-", "", profile)
  fixture <- c(
    spelling = "bad_spelling",
    link = "broken_link",
    `generated-drift` = "generated_drift"
  )[[fixture]]
  selected <- self_tests[self_tests$fixture == fixture, , drop = FALSE]
  protected_after <- documentation_qa_protected_manifest(root)
  protected <- protected_after
  protected$before_sha256 <- protected_before$observed_sha256
  protected$unchanged_during_gate <-
    protected_before$observed_sha256 == protected_after$observed_sha256
  protected_failures <- sum(
    !protected$exact | !protected$unchanged_during_gate
  )
  written <- documentation_qa_write_negative(
    target,
    profile,
    selected,
    self_tests,
    protected,
    toolchain
  )
  print(written$summary, row.names = FALSE)
  cat("Artifacts: ", normalizePath(target), "\n", sep = "")
  if (written$detected && protected_failures == 0L) 1L else 2L
}

documentation_qa_gate_main <- function(root, target, url_mode, toolchain) {
  protected_before <- documentation_qa_protected_manifest(root)
  source_before <- documentation_qa_source_manifest(root)
  work_dir <- tempfile("sitemix-documentation-qa-work-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  built <- documentation_qa_build_source(root, work_dir)
  wordlist <- documentation_qa_wordlist(built$package_dir)
  spelling <- documentation_qa_spelling(built$package_dir)
  urls <- documentation_qa_url_inventory(built$package_dir)
  online <- documentation_qa_online_urls(
    built$package_dir,
    urls$database,
    url_mode
  )
  writeLines(
    online$log,
    file.path(work_dir, "url-online.log"),
    useBytes = TRUE
  )
  rd <- documentation_qa_rd(built$package_dir)
  pkgdown_result <- documentation_qa_pkgdown(
    built$package_dir,
    file.path(work_dir, "pkgdown.log")
  )
  pages <- documentation_qa_required_pages(pkgdown_result$site_dir)
  links <- documentation_qa_site_links(pkgdown_result$site_dir)
  self_tests <- documentation_qa_self_tests(work_dir)

  doc_child <- documentation_qa_run_child(
    root,
    "audit-documentation-drift.R",
    file.path(work_dir, "documentation-drift"),
    file.path(work_dir, "documentation-drift-child.log")
  )
  vignette_child <- documentation_qa_run_child(
    root,
    "audit-vignette-reproducibility.R",
    file.path(work_dir, "vignette"),
    file.path(work_dir, "vignette-child.log")
  )
  doc_summary_path <- file.path(
    doc_child$output,
    "documentation-summary.csv"
  )
  vignette_summary_path <- file.path(
    vignette_child$output,
    "vignette-summary.csv"
  )
  if (!file.exists(doc_summary_path) || !file.exists(vignette_summary_path)) {
    stop("Documentation QA child evidence is incomplete.", call. = FALSE)
  }
  doc_summary <- utils::read.csv(doc_summary_path, stringsAsFactors = FALSE)
  vignette_summary <- utils::read.csv(
    vignette_summary_path,
    stringsAsFactors = FALSE
  )
  coupling <- utils::read.csv(
    file.path(doc_child$output, "documentation-coupling-active.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  protected_after <- documentation_qa_protected_manifest(root)
  protected <- protected_after
  protected$before_sha256 <- protected_before$observed_sha256
  protected$unchanged_during_gate <-
    protected_before$observed_sha256 == protected_after$observed_sha256
  protected_failures <- sum(
    !protected$exact | !protected$unchanged_during_gate
  ) + doc_summary$protected_failures + vignette_summary$protected_failures

  url_occurrences <- nrow(urls$inventory)
  https <- urls$inventory$scheme == "https"
  online_https_checked <- sum(
    online$targets$scheme == "https" &
      online$targets$checked & online$targets$pass
  )
  online_failures <- nrow(online$issues)
  offline_failures <- nrow(urls$offline_issues)
  site_overlay_failures <- sum(
    !built$site_overlay$expected_path |
      !built$site_overlay$git_tracked | !built$site_overlay$exact
  )
  local_links <- sum(links$link_kind == "local")
  html_files <- length(list.files(
    pkgdown_result$site_dir,
    pattern = "[.]html$",
    recursive = TRUE
  ))
  observed <- c(
    wordlist_entries = wordlist$contract$entries,
    spelling_findings = nrow(spelling),
    url_occurrences = url_occurrences,
    url_unique = length(unique(urls$inventory$url)),
    https_occurrences = sum(https),
    https_unique = length(unique(urls$inventory$url[https])),
    online_https_checked = online_https_checked,
    url_parent_files = length(unique(urls$inventory$parent)),
    url_failures = offline_failures + online_failures,
    rd_files = length(rd$files),
    rd_syntax_diagnostics = sum(rd$diagnostics$check == "syntax"),
    rd_content_diagnostics = sum(rd$diagnostics$check == "content"),
    rd_xrefs = nrow(rd$xrefs),
    rd_xref_files = length(unique(rd$xrefs$file)),
    rd_xref_failures = nrow(rd$xref_issues),
    required_site_pages = nrow(pages),
    required_site_page_failures = sum(!pages$exists),
    site_html_files = html_files,
    site_local_links = local_links,
    site_link_failures = sum(!links$pass),
    site_overlay_failures = site_overlay_failures,
    pkgdown_warnings = length(pkgdown_result$warnings),
    pkgdown_errors = length(pkgdown_result$errors),
    doc_generated_drift = doc_summary$generated_drift,
    doc_active_coupling = doc_summary$active_coupling,
    vignette_source_failures = vignette_summary$source_contract_failures,
    vignette_render_failures = vignette_summary$render_failures,
    vignette_link_failures = vignette_summary$link_failures,
    canonical_vignettes = vignette_summary$canonical_vignettes,
    protected_failures = protected_failures
  )
  budget <- documentation_qa_budget(root, observed)
  wordlist_pass <- with(wordlist$contract, {
    empty_entries == 0L && duplicate_entries == 0L &&
      reviewed_sha256_exact && stale_consumer_terms == 0L
  })
  self_test_pass <- all(self_tests$detected) &&
    all(!self_tests$repository_modified)
  source_manifest <- documentation_qa_source_manifest(root)
  if (!identical(source_manifest$path, source_before$path)) {
    stop("Documentation QA source manifest path order drifted.", call. = FALSE)
  }
  source_manifest$before_size_bytes <- source_before$size_bytes
  source_manifest$before_md5 <- source_before$md5
  source_manifest$before_sha256 <- source_before$sha256
  source_manifest$unchanged_during_gate <-
    source_manifest$size_bytes == source_manifest$before_size_bytes &
    source_manifest$md5 == source_manifest$before_md5 &
    source_manifest$sha256 == source_manifest$before_sha256
  source_stability_failures <- sum(!source_manifest$unchanged_during_gate)
  components <- do.call(rbind, list(
    documentation_qa_component_row(
      "spelling_and_wordlist",
      nrow(spelling) == 0L && wordlist_pass,
      paste0(nrow(spelling), " findings; ", wordlist$contract$entries, " words")
    ),
    documentation_qa_component_row(
      "url_inventory_and_check",
      identical(online$status, "PASS") &&
        offline_failures + online_failures == 0L,
      paste0(
        url_occurrences, " occurrences; mode=", url_mode,
        "; online_https_checked=", online_https_checked
      )
    ),
    documentation_qa_component_row(
      "rd_syntax_content_xrefs",
      !nrow(rd$diagnostics) && !nrow(rd$xref_issues),
      paste0(length(rd$files), " Rd files; ", nrow(rd$xrefs), " xrefs")
    ),
    documentation_qa_component_row(
      "pkgdown_clean_build",
      pkgdown_result$pass && !sum(!pages$exists),
      paste0(html_files, " HTML files; ", nrow(pages), " required")
    ),
    documentation_qa_component_row(
      "site_overlay_integrity",
      site_overlay_failures == 0L,
      paste0(
        nrow(built$site_overlay), " overlay manifest rows; ",
        site_overlay_failures, " failures"
      )
    ),
    documentation_qa_component_row(
      "pkgdown_local_link_graph",
      !sum(!links$pass),
      paste0(local_links, " local references")
    ),
    documentation_qa_component_row(
      "documentation_drift_child",
      identical(doc_child$status, 0L) && identical(doc_summary$status, "PASS"),
      paste0(doc_summary$generated_files, " generated files")
    ),
    documentation_qa_component_row(
      "vignette_reproducibility_child",
      identical(vignette_child$status, 0L) &&
        identical(vignette_summary$status, "PASS"),
      paste0(vignette_summary$render_passes, "/17 renders")
    ),
    documentation_qa_component_row(
      "negative_controls",
      self_test_pass,
      paste0(sum(self_tests$detected), "/3 detected")
    ),
    documentation_qa_component_row(
      "maintainer_source_stability",
      source_stability_failures == 0L,
      paste0(
        nrow(source_manifest), " source manifest rows; ",
        source_stability_failures, " concurrent changes"
      )
    ),
    documentation_qa_component_row(
      "protected_identities",
      protected_failures == 0L,
      paste0(protected_failures, " failures")
    )
  ))
  status <- if (
    all(budget$pass) && all(components$status == "PASS") &&
      wordlist_pass && self_test_pass
  ) "PASS" else "FAIL"
  summary <- data.frame(
    status = status,
    profile = "gate",
    url_mode = url_mode,
    validation_boundary = "source_tar_plus_tracked_site_overlay",
    child_validation_boundary = "maintainer_source_children",
    budget_rows = nrow(budget),
    budget_failures = sum(!budget$pass),
    component_failures = sum(components$status != "PASS"),
    spelling_findings = nrow(spelling),
    wordlist_entries = wordlist$contract$entries,
    url_occurrences = url_occurrences,
    url_unique = length(unique(urls$inventory$url)),
    https_unique = length(unique(urls$inventory$url[https])),
    online_https_checked = online_https_checked,
    url_failures = offline_failures + online_failures,
    rd_files = length(rd$files),
    rd_xrefs = nrow(rd$xrefs),
    rd_failures = nrow(rd$diagnostics) + nrow(rd$xref_issues),
    pkgdown_html_files = html_files,
    pkgdown_site_files = length(list.files(
      pkgdown_result$site_dir,
      recursive = TRUE,
      all.files = TRUE
    )),
    pkgdown_local_links = local_links,
    pkgdown_link_failures = sum(!links$pass),
    required_page_failures = sum(!pages$exists),
    site_overlay_files = nrow(built$site_overlay),
    site_overlay_failures = site_overlay_failures,
    documentation_drift = doc_summary$generated_drift,
    active_coupling = doc_summary$active_coupling,
    vignette_render_passes = vignette_summary$render_passes,
    vignette_render_failures = vignette_summary$render_failures,
    vignette_expected_warnings = vignette_summary$expected_warnings,
    vignette_unexpected_warnings = vignette_summary$unexpected_warnings,
    configured_redirect_passes = vignette_summary$redirect_passes,
    negative_controls_detected = sum(self_tests$detected),
    source_stability_failures = source_stability_failures,
    protected_failures = protected_failures,
    source_tarball_sha256 = built$tarball_sha256,
    stringsAsFactors = FALSE
  )
  artifact_dir <- documentation_qa_write_outputs(
    target,
    summary,
    components,
    budget,
    spelling,
    wordlist,
    urls$inventory,
    urls$offline_issues,
    online$issues,
    online$targets,
    rd$diagnostics,
    rd$xrefs,
    rd$xref_issues,
    pages,
    links,
    coupling,
    self_tests,
    protected,
    toolchain,
    source_manifest,
    built,
    pkgdown_result,
    doc_child,
    vignette_child,
    work_dir
  )
  print(summary, row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")
  if (identical(status, "PASS")) 0L else 1L
}

documentation_qa_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  documentation_qa_validate_args(args)
  root <- documentation_qa_root()
  package <- unname(read.dcf(file.path(root, "DESCRIPTION"))[1L, "Package"])
  if (!identical(package, "sitemix")) {
    stop("Documentation QA requires the sitemix source tree.", call. = FALSE)
  }
  target <- documentation_qa_output_target(
    documentation_qa_arg_value(args, "out-dir")
  )
  profile <- documentation_qa_arg_value(args, "profile", "gate")
  url_mode <- documentation_qa_arg_value(args, "url-mode", "online")
  toolchain <- documentation_qa_require_packages(root)
  if (!identical(profile, "gate")) {
    return(documentation_qa_negative_main(
      root,
      target,
      profile,
      toolchain
    ))
  }
  documentation_qa_gate_main(root, target, url_mode, toolchain)
}

documentation_qa_entry <- function() {
  tryCatch(
    documentation_qa_main(),
    error = function(error) {
      message("documentation QA contract error: ", conditionMessage(error))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = documentation_qa_entry(), runLast = FALSE)
}
