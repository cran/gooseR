# Test script for gooseR essential functions
# Date: 2025-12-03

library(gooseR)
library(cli)

cli::cli_h1("Testing gooseR Essential Functions")

# Test 1: goose_give_sample()
cli::cli_h2("Test 1: goose_give_sample()")
test_data <- data.frame(
  id = 1:10,
  value = rnorm(10),
  category = sample(letters[1:3], 10, replace = TRUE),
  date = Sys.Date() + 1:10
)

# Share the test data
goose_give_sample(test_data, "test_dataset")

# Test with mtcars
cli::cli_alert_info("Testing with mtcars dataset")
goose_give_sample(mtcars, rows = 3)

# Test 2: goose_make_a_plan()
cli::cli_h2("Test 2: goose_make_a_plan()")
plan <- goose_make_a_plan(focus = "predictive")

# Test 3: goose_loop_me()
cli::cli_h2("Test 3: goose_loop_me()")

# Simple code to loop
simple_code <- "df <- read.csv(file)
summary(df)
write.csv(df, paste0('processed_', file))"

cli::cli_alert_info("Converting to file loop:")
goose_loop_me(simple_code, loop_over = "files")

cli::cli_alert_info("\nConverting to parallel processing:")
goose_loop_me(simple_code, loop_over = "files", parallel = TRUE)

# Test 4: goose_honk()
cli::cli_h2("Test 4: goose_honk()")

# Gentle review
cli::cli_alert_info("Gentle review:")
goose_honk(severity = "gentle", focus = "statistics")

# Harsh review
cli::cli_alert_info("\nHarsh review:")
goose_honk(severity = "harsh", focus = "performance")

# Test 5: goose_continuation_prompt()
cli::cli_h2("Test 5: goose_continuation_prompt()")

# Generate continuation prompt
prompt_file <- goose_continuation_prompt(
  include_files = TRUE,
  include_todos = TRUE
)

cli::cli_alert_success("All essential functions tested successfully!")
cli::cli_alert_info("Continuation prompt saved to: {.file {prompt_file}}")
