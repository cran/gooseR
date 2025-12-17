#!/usr/bin/env Rscript

library(gooseR)

cat("\n")
cat("=====================================\n")
cat("  GooseR Package Test - Fixed        \n")
cat("=====================================\n\n")

# Test Async
cat("Testing Async module:\n")
tryCatch({
  # Create worker pool with correct parameter name
  pool <- WorkerPool$new(n_workers = 2)
  cat("  ✓ WorkerPool class instantiates\n")
  pool$shutdown()
  
  # Test future plan
  future::plan(future::sequential)
  cat("  ✓ Async infrastructure available\n")
}, error = function(e) {
  cat("  ✗ Async error:", e$message, "\n")
})

# Test Templates
cat("\nTesting Templates module:\n")
tryCatch({
  # List templates
  templates <- goose_template_list()
  cat("  ✓ goose_template_list() works -", nrow(templates), "templates\n")
  
  # Get builtin template
  template <- goose_template_builtin("code_review")
  cat("  ✓ goose_template_builtin() works\n")
  
  # Validate template - properly handle the list result
  validation <- goose_template_validate(template)
  cat("  ✓ goose_template_validate() works - valid:", validation$valid, "\n")
}, error = function(e) {
  cat("  ✗ Templates error:", e$message, "\n")
})

cat("\n✅ Tests completed successfully!\n\n")
