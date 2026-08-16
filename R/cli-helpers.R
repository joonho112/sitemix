# CLI formatting helpers ----------------------------------------------------

.sm_cli_code <- function(x) {
  x <- as.character(x)
  vapply(
    x,
    function(value) cli::format_inline("{.code {value}}"),
    character(1),
    USE.NAMES = FALSE
  )
}

.sm_cli_collapse <- function(x, quote = FALSE) {
  if (length(x) == 0L) {
    return("")
  }

  x <- as.character(x)
  if (quote) {
    x <- .sm_cli_code(x)
  }
  if (length(x) == 1L) {
    x
  } else if (length(x) == 2L) {
    paste(x, collapse = " and ")
  } else {
    paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[[length(x)]])
  }
}

.sm_cli_expected_actual <- function(expected, actual) {
  c(
    x = paste0("Expected ", .sm_cli_collapse(expected, quote = TRUE), "."),
    i = paste0("Actual: ", .sm_cli_collapse(actual, quote = TRUE), ".")
  )
}

.sm_cli_row_identity <- function(row_identity) {
  if (is.null(row_identity) || length(row_identity) == 0L) {
    return("")
  }

  if (is.null(names(row_identity)) || any(names(row_identity) == "")) {
    stop("`row_identity` must be a named list or vector.", call. = FALSE)
  }

  values <- vapply(
    row_identity,
    function(value) {
      if (length(value) != 1L) {
        value <- paste(value, collapse = ", ")
      }
      if (is.character(value)) {
        encodeString(value, quote = '"')
      } else {
        as.character(value)
      }
    },
    character(1)
  )
  paste0(names(row_identity), " = ", values, collapse = ", ")
}

.sm_cli_condition_body <- function(
  expected = NULL,
  actual = NULL,
  row_identity = NULL,
  location = NULL,
  fix = NULL
) {
  body <- character()

  if (!is.null(row_identity)) {
    body <- c(body, i = paste0("Row identity: ", .sm_cli_row_identity(row_identity), "."))
  }
  if (!is.null(location)) {
    body <- c(body, i = paste0("Location: ", .sm_cli_row_identity(location), "."))
  }
  if (!is.null(expected)) {
    body <- c(body, x = paste0("Expected ", .sm_cli_collapse(expected, quote = TRUE), "."))
  }
  if (!is.null(actual)) {
    body <- c(body, i = paste0("Actual: ", .sm_cli_collapse(actual, quote = TRUE), "."))
  }
  if (!is.null(fix)) {
    body <- c(body, i = paste0("Fix: ", fix))
  }

  if (length(body) == 0L) {
    NULL
  } else {
    body
  }
}

.sm_cli_missing_columns <- function(columns) {
  noun <- if (length(columns) == 1L) "column" else "columns"
  paste0(
    "Missing required ",
    noun,
    ": ",
    .sm_cli_collapse(columns, quote = TRUE),
    "."
  )
}

.sm_cli_fix <- function(fix) {
  c(i = paste0("Fix: ", fix))
}
