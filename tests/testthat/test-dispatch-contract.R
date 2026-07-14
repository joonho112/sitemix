dispatch_contract_table <- function() {
  utils::read.csv(
    testthat::test_path("_data", "dispatch", "dispatch-contract.csv"),
    stringsAsFactors = FALSE,
    na.strings = ""
  )
}

dispatch_contract_input <- function(input_shape, family) {
  if (identical(input_shape, "student")) {
    if (identical(family, "binomial")) {
      return(list(
        data = data.frame(
          site_id = rep("S1", 4L), year = rep(2024L, 4L),
          y = c(1L, 0L, 1L, 0L)
        ),
        indicator = "y"
      ))
    }
    if (identical(family, "multivariate")) {
      return(list(
        data = data.frame(
          site_id = rep("S1", 4L), year = rep(2024L, 4L),
          a = c(1L, 1L, 0L, 0L), b = c(1L, 0L, 1L, 0L)
        ),
        indicators = c("a", "b")
      ))
    }
    return(list(
      data = data.frame(
        site_id = rep("S1", 4L), year = rep(2024L, 4L),
        category = c("A", "A", "B", "B"),
        stringsAsFactors = FALSE
      ),
      indicator = "category"
    ))
  }

  if (identical(input_shape, "counts")) {
    if (identical(family, "binomial")) {
      return(list(
        data = data.frame(site_id = "S1", year = 2024L, n_jt = 8L, c_jt_y = 3L),
        indicator = "y"
      ))
    }
    if (identical(family, "multivariate")) {
      return(list(
        data = data.frame(
          site_id = "S1", year = 2024L, n_jt = 8L,
          c_jt_a = 4L, c_jt_b = 5L, c_jt_a_b = 3L
        ),
        indicators = c("a", "b")
      ))
    }
    return(list(
      data = data.frame(
        site_id = "S1", year = 2024L, n_jt = 8L,
        c_jt_A = 3L, c_jt_B = 5L
      )
    ))
  }

  if (identical(family, "binomial")) {
    return(list(
      data = data.frame(
        site_id = "S1", year = 2024L, indicator = "y",
        c_jt = 3L, n_jt = 8L, stringsAsFactors = FALSE
      ),
      indicator = "y"
    ))
  }

  list(
    data = data.frame(
      site_id = c("S1", "S1"), year = c(2024L, 2024L),
      indicator = c("a", "b"), c_jt = c(3L, 5L), n_jt = c(8L, 8L),
      stringsAsFactors = FALSE
    ),
    sampling_relation = "same_units"
  )
}

dispatch_contract_call <- function(input_shape, family, direct = FALSE, ...) {
  args <- c(
    dispatch_contract_input(input_shape, family),
    list(family = family, vjt = TRUE, min_n = 1L),
    list(...)
  )

  if (identical(input_shape, "student")) {
    fun <- sitemix::sm_estimate
  } else if (identical(input_shape, "counts")) {
    if (isTRUE(direct)) {
      fun <- sitemix::sm_estimate
      args$from_counts <- TRUE
    } else {
      fun <- sitemix::sm_estimate_from_counts
    }
  } else if (isTRUE(direct)) {
    fun <- sitemix::sm_estimate
    args$from_aggregates <- TRUE
  } else {
    fun <- sitemix::sm_estimate_from_aggregates
  }

  withCallingHandlers(
    do.call(fun, args),
    warning = function(cnd) invokeRestart("muffleWarning")
  )
}

dispatch_contract_scenario <- function(x) {
  aggregate_case <- attr(x, "aggregate_case", exact = TRUE)
  if (!is.null(aggregate_case)) {
    return(aggregate_case)
  }
  switch(
    attr(x, "family", exact = TRUE),
    binomial = "A",
    multivariate = "B",
    multinomial = "C"
  )
}

test_that("machine-readable dispatch truth table covers the 3 by 3 surface", {
  contract <- dispatch_contract_table()

  expect_named(
    contract,
    c(
      "input_shape", "family", "legal", "scenario",
      "observable_input_mode", "public_entry", "required_shape",
      "condition_class"
    )
  )
  expect_equal(nrow(contract), 9L)
  expect_setequal(contract$input_shape, c("student", "counts", "aggregate"))
  expect_setequal(contract$family, c("binomial", "multivariate", "multinomial"))
  expect_equal(
    nrow(unique(contract[c("input_shape", "family")])),
    nrow(contract)
  )
  expect_equal(sum(contract$legal), 8L)
  expect_equal(
    contract$public_entry,
    c(
      rep("sm_estimate", 3L),
      rep("sm_estimate_from_counts", 3L),
      rep("sm_estimate_from_aggregates", 3L)
    )
  )
  expect_true(all(!is.na(contract$required_shape) & nzchar(contract$required_shape)))
  expect_equal(
    contract$condition_class[!contract$legal],
    "sitemix_error_ambiguous_dispatch"
  )
})

test_that("every legal dispatch cell reaches exactly its documented scenario", {
  contract <- dispatch_contract_table()
  legal <- contract[contract$legal, , drop = FALSE]

  for (i in seq_len(nrow(legal))) {
    row <- legal[i, ]
    out <- dispatch_contract_call(row$input_shape, row$family)

    expect_s3_class(out, "sitemix_estimates")
    expect_equal(unique(out$input_mode), row$observable_input_mode)
    expect_equal(attr(out, "family", exact = TRUE), row$family)
    expect_equal(dispatch_contract_scenario(out), row$scenario)
    expect_true("V" %in% names(out))
  }
})

test_that("count and aggregate wrappers are observationally equal to direct dispatch", {
  contract <- dispatch_contract_table()
  wrapper_cells <- contract[
    contract$legal & contract$input_shape %in% c("counts", "aggregate"),
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(wrapper_cells))) {
    row <- wrapper_cells[i, ]
    wrapper <- dispatch_contract_call(row$input_shape, row$family)
    direct <- dispatch_contract_call(row$input_shape, row$family, direct = TRUE)

    expect_identical(wrapper, direct)
  }
})

test_that("aggregate multinomial is a stable classed illegal dispatch", {
  contract <- dispatch_contract_table()
  row <- contract[contract$input_shape == "aggregate" & contract$family == "multinomial", ]

  for (direct in c(FALSE, TRUE)) {
    err <- tryCatch(
      dispatch_contract_call(row$input_shape, row$family, direct = direct),
      error = identity
    )
    expect_s3_class(err, row$condition_class)
    expect_s3_class(err, "sitemix_error_aggregate")
    expect_equal(
      err$expected,
      c("family = \"binomial\" for D0", "family = \"multivariate\" for D1")
    )
    expect_equal(err$actual, "multinomial")
    expect_match(err$fix, "full sufficient counts", fixed = TRUE)
  }
})

test_that("aggregate schema mismatches retain exact classed D0 and D1 guards", {
  d1_shape <- dispatch_contract_input("aggregate", "multivariate")$data
  d0_shape <- dispatch_contract_input("aggregate", "binomial")$data

  cases <- list(
    list(
      data = d1_shape,
      family = "binomial",
      expected = "one retained indicator / aggregate_case D0",
      actual = "D1",
      fix = "Use a single indicator for D0, or use the multivariate aggregate path for D1 working-independence."
    ),
    list(
      data = d0_shape,
      family = "multivariate",
      expected = "two or more retained marginal indicators / aggregate_case D1",
      actual = "D0",
      fix = "Use `family = \"binomial\"` or `aggregate_case = \"D0\"` for single-indicator aggregate input."
    )
  )

  for (case in cases) {
    for (direct in c(FALSE, TRUE)) {
      args <- list(data = case$data, family = case$family)
      fun <- sitemix::sm_estimate_from_aggregates
      if (isTRUE(direct)) {
        fun <- sitemix::sm_estimate
        args$from_aggregates <- TRUE
      }
      err <- tryCatch(do.call(fun, args), error = identity)

      expect_s3_class(err, "sitemix_error_ambiguous_dispatch")
      expect_s3_class(err, "sitemix_error_aggregate")
      expect_equal(err$expected, case$expected)
      expect_equal(err$actual, case$actual)
      expect_equal(err$fix, case$fix)
    }
  }
})

test_that("Anscombe is arcsine-only in every legal dispatch cell", {
  contract <- dispatch_contract_table()
  legal <- contract[contract$legal, , drop = FALSE]

  for (i in seq_len(nrow(legal))) {
    row <- legal[i, ]
    for (vst in c("none", "logit")) {
      err <- tryCatch(
        dispatch_contract_call(
          row$input_shape,
          row$family,
          anscombe = TRUE,
          vst = vst
        ),
        error = identity
      )
      expect_s3_class(err, "sitemix_error_anscombe_requires_arcsine")
      expect_s3_class(err, "sitemix_error_argument")
      expect_equal(err$expected, "vst = \"arcsine\"")
      expect_equal(err$actual, paste0("vst = \"", vst, "\""))
      expect_match(err$fix, "anscombe = FALSE", fixed = TRUE)
    }
  }
})
