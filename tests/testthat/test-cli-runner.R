test_that(".goose_cli_run retries once on timeout error", {
  calls <- 0
  old <- getOption("goose.internal_process_run")
  on.exit(options(goose.internal_process_run = old), add = TRUE)

  options(goose.internal_process_run = function(command, args, timeout) {
    calls <<- calls + 1
    if (calls == 1) stop("timeout")
    list(status = 0L, stdout = "ok", stderr = "")
  })

  res <- gooseR:::`.goose_cli_run`(args = c("--version"), timeout = 1, retries = 1, quiet = TRUE)
  expect_equal(res$status, 0L)
  expect_equal(calls, 2)
})

test_that(".goose_cli_run does not retry when retries=0", {
  calls <- 0
  old <- getOption("goose.internal_process_run")
  on.exit(options(goose.internal_process_run = old), add = TRUE)

  options(goose.internal_process_run = function(command, args, timeout) {
    calls <<- calls + 1
    stop("timeout")
  })

  res <- gooseR:::`.goose_cli_run`(args = c("--version"), timeout = 1, retries = 0, quiet = TRUE)
  expect_equal(res$status, 1L)
  expect_equal(calls, 1)
})
