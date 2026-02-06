# Internal Goose CLI runner utilities
#
# Centralized subprocess execution with:
# - consistent stdout/stderr capture
# - timeouts (including safe handling of Inf)
# - retries for transient failures (timeouts, network issues, etc.)
#
# NOTE: Not exported.

#' @keywords internal
#' @noRd
.goose_process_run <- function(command, args, timeout = Inf) {
  # Allow injection for tests.
  injected <- getOption("goose.internal_process_run", NULL)
  if (is.function(injected)) {
    return(injected(command = command, args = args, timeout = timeout))
  }

  # processx::run() accepts a numeric timeout in seconds.
  # Treat Inf/NULL as "no timeout".
  if (is.null(timeout) || isTRUE(is.infinite(timeout))) {
    timeout <- Inf
  }

  processx::run(
    command = command,
    args = args,
    echo_cmd = FALSE,
    echo = FALSE,
    error_on_status = FALSE,
    timeout = timeout
  )
}

#' Decide whether an error/status is retryable
#'
#' @param status Integer exit status (0 = success)
#' @param stderr Character stderr combined
#' @param error_msg Optional error message (e.g., from timeout)
#' @param retry_on Character vector of classes to retry on
#' @keywords internal
#' @noRd
.goose_is_retryable <- function(status, stderr = "", error_msg = NULL,
                               retry_on = c("timeout", "transient")) {
  stderr_l <- tolower(paste(stderr, collapse = "\n"))
  err_l <- tolower(paste(if (is.null(error_msg)) "" else error_msg, collapse = "\n"))
  combined <- paste(stderr_l, err_l)

  is_timeout <- grepl("timeout|timed out", combined, fixed = FALSE)

  # Heuristic transient bucket: common network/process blips.
  is_transient <- grepl(
    "temporar|try again|unavailable|connection|network|reset|econn|socket|tls|handshake|rate limit|too many requests|429|502|503|504|broken pipe|i/o",
    combined,
    ignore.case = TRUE
  )

  should_retry_timeout <- ("timeout" %in% retry_on) && is_timeout
  should_retry_transient <- ("transient" %in% retry_on) && is_transient

  (status != 0) && (should_retry_timeout || should_retry_transient)
}

#' Run goose CLI with retries/timeouts
#'
#' Returns a list with stdout/stderr/status.
#'
#' @param args Character vector of goose CLI args
#' @param timeout Numeric seconds (Inf allowed)
#' @param retries Integer number of retries after the first attempt (0 = no retry)
#' @param retry_delay Numeric seconds to sleep before retry
#' @param retry_on Character vector ("timeout", "transient")
#' @param quiet Logical; if FALSE, messages are printed on retry
#'
#' @keywords internal
#' @noRd
.goose_cli_run <- function(args,
                           timeout = getOption("goose.timeout", 300),
                           retries = getOption("goose.retries", 1),
                           retry_delay = getOption("goose.retry_delay", 1),
                           retry_on = getOption("goose.retry_on", c("timeout", "transient")),
                           quiet = TRUE) {
  attempt <- 0
  last_error <- NULL

  repeat {
    attempt <- attempt + 1

    res <- tryCatch({
      .goose_process_run("goose", args = args, timeout = timeout)
    }, error = function(e) {
      last_error <<- e
      NULL
    })

    # If we got a processx-like result
    if (!is.null(res)) {
      status <- as.integer(if (is.null(res$status)) 1L else res$status)
      stdout <- if (is.null(res$stdout)) character() else res$stdout
      stderr <- if (is.null(res$stderr)) character() else res$stderr

      # Success
      if (identical(status, 0L)) {
        return(list(stdout = stdout, stderr = stderr, status = status, attempt = attempt))
      }

      # Failure: maybe retry
      # NOTE: `retries` is the number of *additional* attempts after the first.
      # So retries=0 means no retry (max 1 attempt), retries=1 means max 2 attempts, etc.
      if (attempt <= retries && .goose_is_retryable(status, stderr = stderr, retry_on = retry_on)) {
        if (!quiet) {
          message("[gooseR] Goose CLI attempt ", attempt, " failed (status=", status, "). Retrying...")
        }
        Sys.sleep(retry_delay)
        next
      }

      return(list(stdout = stdout, stderr = stderr, status = status, attempt = attempt))
    }

    # If we hit an R-level error (commonly timeout)
    err_msg <- if (!is.null(last_error)) conditionMessage(last_error) else "Unknown error"
    is_retryable_error <- grepl("timeout|timed out", err_msg, ignore.case = TRUE) && ("timeout" %in% retry_on)

    if (attempt <= retries && is_retryable_error) {
      if (!quiet) {
        message("[gooseR] Goose CLI attempt ", attempt, " errored ('", err_msg, "'). Retrying...")
      }
      Sys.sleep(retry_delay)
      next
    }

    # Give back a consistent shape
    return(list(stdout = character(), stderr = err_msg, status = 1L, attempt = attempt))
  }
}
