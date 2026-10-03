pivot_alias_fixture <- function() {
  data.frame(
    site_id = rep(c("S1", "S2"), each = 3L),
    year = rep(2025L, 6L),
    subgroup = c("  all-students  ", "A", "B", "OvEr.All", "A", "B"),
    c_jt = c(3L, 1L, 2L, 4L, 1L, 3L),
    n_jt = c(30L, 10L, 20L, 40L, 10L, 30L),
    stringsAsFactors = FALSE
  )
}

pivot_alias_quiet_d1 <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}

pivot_alias_capture_error <- function(expr) {
  tryCatch(expr, error = identity)
}

test_that("publisher total aliases canonicalize in Framing X and Y round trips", {
  source <- pivot_alias_fixture()

  framing_x <- sitemix::sm_pivot_subgroups_to_sites(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    indicator = "rate"
  )
  expect_true(all(c("S1_ALL", "S2_ALL") %in% framing_x$site_id))
  expect_identical(
    framing_x$source_subgroup[framing_x$site_id %in% c("S1_ALL", "S2_ALL")],
    c("  all-students  ", "OvEr.All")
  )

  estimated_x <- sitemix::sm_estimate_from_aggregates(
    framing_x,
    family = "binomial",
    indicator = "rate",
    aggregate_case = "D0",
    framing = "subgroup_as_site",
    min_n = 1L
  )
  expected_x <- stats::setNames(framing_x$c_jt / framing_x$n_jt, framing_x$site_id)
  actual_x <- stats::setNames(estimated_x$theta_raw, estimated_x$site_id)
  expect_identical(actual_x[names(expected_x)], expected_x)

  framing_y <- sitemix::sm_pivot_subgroups_to_indicators(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    indicator_set = c(" overall ", "B", "A")
  )
  expect_identical(attr(framing_y, "indicator_set"), c("ALL", "B", "A"))
  expect_identical(unique(framing_y$indicator), c("ALL", "B", "A"))
  expect_identical(
    framing_y$source_subgroup[framing_y$indicator == "ALL"],
    c("  all-students  ", "OvEr.All")
  )

  estimated_y <- pivot_alias_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      framing_y,
      family = "multivariate",
      aggregate_case = "D1",
      framing = "subgroup_as_indicator",
      sampling_relation = "different_units",
      indicators = c("ALL", "B", "A"),
      min_n = 1L
    )
  )
  expected_y <- stats::setNames(
    framing_y$c_jt / framing_y$n_jt,
    paste(framing_y$site_id, framing_y$indicator, sep = "/")
  )
  actual_y <- stats::setNames(
    estimated_y$theta_raw,
    paste(estimated_y$site_id, estimated_y$indicator, sep = "/")
  )
  expect_identical(actual_y[names(expected_y)], expected_y)
})

test_that("punctuation case and whitespace aliases resolve one canonical partition total", {
  source <- pivot_alias_fixture()
  denominator <- sitemix::sm_pivot_subgroups_to_sites(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    partition_target = "denominator_composition"
  )
  case <- sitemix::sm_pivot_subgroups_to_sites(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    partition_target = "case_composition"
  )

  expect_identical(attr(denominator, "partition_reference"), "ALL")
  expect_identical(attr(case, "partition_reference"), "ALL")
  expect_identical(attr(denominator, "indicator_order"), c("A", "B"))
  expect_identical(denominator$n_jt, c(30L, 40L))
  expect_identical(denominator$c_jt_A, c(10L, 10L))
  expect_identical(denominator$c_jt_B, c(20L, 30L))
  expect_identical(case$n_jt, c(3L, 4L))
  expect_identical(case$c_jt_A, c(1L, 1L))
  expect_identical(case$c_jt_B, c(2L, 3L))
})

test_that("the fixed publisher total vocabulary maps exactly to ALL", {
  aliases <- c(
    "ALL",
    " all student ",
    "ALL_STUDENTS",
    "allstudents",
    "Total",
    " over-all ",
    "OVERALL"
  )
  source <- data.frame(
    site_id = rep(paste0("S", seq_along(aliases)), each = 3L),
    year = 2025L,
    subgroup = as.vector(rbind(aliases, rep("A", length(aliases)), rep("B", length(aliases)))),
    c_jt = rep(c(3L, 1L, 2L), length(aliases)),
    n_jt = rep(c(30L, 10L, 20L), length(aliases)),
    stringsAsFactors = FALSE
  )

  framing_x <- sitemix::sm_pivot_subgroups_to_sites(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt"
  )
  framing_y <- sitemix::sm_pivot_subgroups_to_indicators(
    source,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt"
  )

  expect_identical(sum(grepl("_ALL$", framing_x$site_id)), length(aliases))
  expect_identical(sum(framing_y$indicator == "ALL"), length(aliases))
  expect_setequal(
    framing_x$source_subgroup[grepl("_ALL$", framing_x$site_id)],
    aliases
  )
  expect_setequal(framing_y$source_subgroup[framing_y$indicator == "ALL"], aliases)
})

test_that("alias collisions fail as normalized duplicate rows and indicators", {
  duplicate <- data.frame(
    site_id = rep("S1", 3L),
    year = rep(2025L, 3L),
    subgroup = c("ALL", " All.Students ", "A"),
    c_jt = c(3L, 3L, 3L),
    n_jt = c(30L, 30L, 30L),
    stringsAsFactors = FALSE
  )

  calls <- list(
    framing_x = quote(sitemix::sm_pivot_subgroups_to_sites(
      duplicate,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt"
    )),
    framing_y = quote(sitemix::sm_pivot_subgroups_to_indicators(
      duplicate,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt"
    )),
    composition = quote(sitemix::sm_pivot_subgroups_to_sites(
      duplicate,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      partition_target = "denominator_composition"
    ))
  )
  for (call_name in names(calls)) {
    err <- pivot_alias_capture_error(eval(calls[[call_name]]))
    expect_true(inherits(err, "sitemix_error_invalid_aggregate_row"), info = call_name)
    expect_true(inherits(err, "sitemix_error"), info = call_name)
    expect_false(is.null(err$expected), info = call_name)
    expect_false(is.null(err$actual), info = call_name)
    expect_true(is.character(err$fix) && nzchar(err$fix), info = call_name)
  }

  set_err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      pivot_alias_fixture(),
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      indicator_set = c("ALL", "total", "A")
    )
  )
  expect_s3_class(set_err, "sitemix_error_invalid_indicators")
  expect_match(set_err$fix, "normalize to `ALL`", fixed = TRUE)
})

test_that("composition targets reject incomplete grids instead of filling zero", {
  incomplete <- pivot_alias_fixture()
  incomplete <- incomplete[!(incomplete$site_id == "S2" & incomplete$subgroup == "B"), ]
  incomplete$n_jt[incomplete$site_id == "S2" & incomplete$subgroup == "OvEr.All"] <- 10L
  incomplete$c_jt[incomplete$site_id == "S2" & incomplete$subgroup == "OvEr.All"] <- 1L

  for (target in c("denominator_composition", "case_composition")) {
    err <- pivot_alias_capture_error(
      sitemix::sm_pivot_subgroups_to_sites(
        incomplete,
        subgroup_col = "subgroup",
        numerator_col = "c_jt",
        denominator_col = "n_jt",
        partition_target = target
      )
    )
    expect_true(inherits(err, "sitemix_error_invalid_partition_target"), info = target)
    expect_identical(err$row_identity, list(site_id = "S2", year = 2025L))
    expect_match(err$actual, "missing = B", fixed = TRUE)
    expect_match(err$fix, "Add the missing category rows explicitly", fixed = TRUE)
  }
})

test_that("suppressed composition rows separate denominator and case claims", {
  suppressed <- pivot_alias_fixture()[1:3, ]
  suppressed$suppression <- c("", "", "Y")
  suppressed$c_jt[suppressed$subgroup == "B"] <- NA_integer_

  denominator <- sitemix::sm_pivot_subgroups_to_sites(
    suppressed,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    suppression_col = "suppression",
    suppression_flag_value = "Y",
    partition_target = "denominator_composition"
  )
  expect_identical(denominator$n_jt, 30L)
  expect_identical(denominator$c_jt_B, 20L)

  case_err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_sites(
      suppressed,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      suppression_col = "suppression",
      suppression_flag_value = "Y",
      partition_target = "case_composition"
    )
  )
  expect_s3_class(case_err, "sitemix_error_invalid_partition_target")
  expect_match(
    conditionMessage(case_err),
    "cannot use publisher-suppressed numerators",
    fixed = TRUE
  )
  expect_identical(case_err$expected, "unsuppressed category numerators")
  expect_identical(
    case_err$row_identity,
    list(site_id = "S1", year = 2025L, indicator = "B")
  )

  suppressed$n_jt[suppressed$subgroup == "B"] <- NA_integer_
  denominator_err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_sites(
      suppressed,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      suppression_col = "suppression",
      suppression_flag_value = "Y",
      partition_target = "denominator_composition"
    )
  )
  expect_s3_class(denominator_err, "sitemix_error_invalid_partition_target")
  expect_match(denominator_err$actual, "missing denominator count", fixed = TRUE)

  suppressed_all <- pivot_alias_fixture()[1:3, ]
  suppressed_all$suppression <- c("Y", "", "")
  suppressed_all$c_jt[1L] <- NA_integer_
  denominator_all <- sitemix::sm_pivot_subgroups_to_sites(
    suppressed_all,
    subgroup_col = "subgroup",
    numerator_col = "c_jt",
    denominator_col = "n_jt",
    suppression_col = "suppression",
    suppression_flag_value = "Y",
    partition_target = "denominator_composition"
  )
  expect_identical(denominator_all$n_jt, 30L)
  case_all_err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_sites(
      suppressed_all,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      suppression_col = "suppression",
      suppression_flag_value = "Y",
      partition_target = "case_composition"
    )
  )
  expect_s3_class(case_all_err, "sitemix_error_invalid_partition_target")
  expect_match(
    conditionMessage(case_all_err),
    "require an unsuppressed `ALL` numerator",
    fixed = TRUE
  )
  expect_identical(case_all_err$expected, "unsuppressed `ALL` case total")
})

test_that("partition mismatch and mixed-level declarations fail deterministically", {
  mismatch <- pivot_alias_fixture()[1:3, ]
  mismatch$n_jt[mismatch$subgroup == "  all-students  "] <- 31L
  mismatch_err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_sites(
      mismatch,
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      partition_target = "denominator_composition"
    )
  )
  expect_s3_class(mismatch_err, "sitemix_error_invalid_partition_target")
  expect_match(mismatch_err$actual, "residual = 1", fixed = TRUE)

  mixed <- pivot_alias_fixture()
  mixed$rtype <- rep(c("school", "district"), each = 3L)
  declarations <- list(
    list(rtype_col = "rtype"),
    list(level_override = list(school = "ALL")),
    list(rtype_col = "rtype", level_override = list(school = "ALL"))
  )
  for (declaration in declarations) {
    err <- pivot_alias_capture_error(do.call(
      sitemix::sm_pivot_subgroups_to_sites,
      c(
        list(
          data = mixed,
          subgroup_col = "subgroup",
          numerator_col = "c_jt",
          denominator_col = "n_jt"
        ),
        declaration
      )
    ))
    expect_s3_class(err, "sitemix_error_invalid_level_override")
    expect_identical(
      err$expected,
      "`level_override = NULL` and `rtype_col = NULL`"
    )
    expect_match(err$actual, "supplied:", fixed = TRUE)
    expect_match(err$fix, "homogeneous reporting level", fixed = TRUE)
  }
})

test_that("partition vocabulary remains frozen and precedes mixed-level routing", {
  err <- pivot_alias_capture_error(
    sitemix::sm_pivot_subgroups_to_sites(
      pivot_alias_fixture(),
      subgroup_col = "subgroup",
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      partition_target = "denominator",
      level_override = list(school = "ALL")
    )
  )

  expect_s3_class(err, "sitemix_error_invalid_partition_target")
  expect_s3_class(err, "sitemix_error_argument")
  expect_identical(
    err$expected,
    c("none", "denominator_composition", "case_composition")
  )
  expect_identical(err$actual, "denominator")
  expect_match(
    err$fix,
    "none, denominator_composition, case_composition",
    fixed = TRUE
  )
})
