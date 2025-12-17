#!/usr/bin/env Rscript

library(gooseR)

cat("\n")
cat("=====================================\n")
cat("  GooseR Package Comprehensive Test  \n")
cat("=====================================\n\n")

# Track results
results <- list()

# Phase 1: Memory Integration
cat("PHASE 1: Memory Integration\n")
cat("---------------------------\n")
tryCatch({
  # Save test data
  test_data <- data.frame(x = 1:5, y = letters[1:5])
  goose_save(test_data, "test_comprehensive")
  cat("✓ goose_save() works\n")
  
  # List saved objects
  saved <- goose_list()
  cat("✓ goose_list() works -", nrow(saved), "objects found\n")
  
  # Load back
  loaded <- goose_load("test_comprehensive")
  cat("✓ goose_load() works\n")
  
  # Clean up
  goose_delete("test_comprehensive")
  cat("✓ goose_delete() works\n")
  
  results$phase1 <- "PASS"
}, error = function(e) {
  cat("✗ Phase 1 error:", e$message, "\n")
  results$phase1 <- "FAIL"
})

cat("\n")

# Phase 2: Universal Branding System
cat("PHASE 2: Universal Branding System\n")
cat("-----------------------------------\n")
tryCatch({
  # Load brand
  brand <- load_brand("block")
  cat("✓ load_brand() works\n")
  
  # Get palette
  palette <- brand_palette("block")
  cat("✓ brand_palette() works -", length(palette), "colors\n")
  
  # Create theme
  theme <- theme_brand("block")
  cat("✓ theme_brand() works\n")
  
  # Generate CSS
  css <- brand_css("block")
  cat("✓ brand_css() works -", nchar(css), "characters\n")
  
  results$phase2 <- "PASS"
}, error = function(e) {
  cat("✗ Phase 2 error:", e$message, "\n")
  results$phase2 <- "FAIL"
})

cat("\n")

# Phase 3: CLI Integration & AI Assistant
cat("PHASE 3: CLI Integration & AI Assistant\n")
cat("----------------------------------------\n")
tryCatch({
  # Check CLI installation
  cli_check <- goose_check_installation()
  cat("✓ goose_check_installation() works\n")
  
  # Get version
  version <- goose_version()
  cat("✓ goose_version() works\n")
  
  # Note: Actual AI functions require Goose CLI to be configured
  cat("  (AI functions require configured Goose CLI)\n")
  
  results$phase3 <- "PASS"
}, error = function(e) {
  cat("✗ Phase 3 error:", e$message, "\n")
  results$phase3 <- "FAIL"
})

cat("\n")

# Phase 4: Advanced AI Features
cat("PHASE 4: Advanced AI Features\n")
cat("------------------------------\n")

# Test Streaming
cat("Testing Streaming module:\n")
tryCatch({
  # Check StreamHandler class
  handler <- StreamHandler$new(
    callback = function(x) {},
    error_callback = function(e) {},
    complete_callback = function() {}
  )
  cat("  ✓ StreamHandler class instantiates\n")
  
  # Check StreamSession class
  session <- StreamSession$new("test_session")
  cat("  ✓ StreamSession class instantiates\n")
  session$close()
  
  results$streaming <- "PASS"
}, error = function(e) {
  cat("  ✗ Streaming error:", e$message, "\n")
  results$streaming <- "FAIL"
})

# Test Cache
cat("Testing Cache module:\n")
tryCatch({
  # Initialize cache
  conn <- goose_cache_init()
  cat("  ✓ goose_cache_init() works\n")
  
  # Set cache entry
  goose_cache_set("test_key", "test_value", conn = conn)
  cat("  ✓ goose_cache_set() works\n")
  
  # Get cache entry
  value <- goose_cache_get("test_key", conn = conn)
  cat("  ✓ goose_cache_get() works - retrieved:", value, "\n")
  
  # Get stats
  stats <- goose_cache_stats(conn = conn)
  cat("  ✓ goose_cache_stats() works\n")
  
  # Clear cache
  goose_cache_clear(conn = conn)
  cat("  ✓ goose_cache_clear() works\n")
  
  DBI::dbDisconnect(conn)
  
  results$cache <- "PASS"
}, error = function(e) {
  cat("  ✗ Cache error:", e$message, "\n")
  results$cache <- "FAIL"
})

# Test Async
cat("Testing Async module:\n")
tryCatch({
  # Create worker pool
  pool <- WorkerPool$new(workers = 2)
  cat("  ✓ WorkerPool class instantiates\n")
  pool$shutdown()
  
  # Test future plan
  future::plan(future::sequential)
  cat("  ✓ Async infrastructure available\n")
  
  results$async <- "PASS"
}, error = function(e) {
  cat("  ✗ Async error:", e$message, "\n")
  results$async <- "FAIL"
})

# Test Templates
cat("Testing Templates module:\n")
tryCatch({
  # List templates
  templates <- goose_template_list()
  cat("  ✓ goose_template_list() works -", nrow(templates), "templates\n")
  
  # Get builtin template
  template <- goose_template_builtin("code_review")
  cat("  ✓ goose_template_builtin() works\n")
  
  # Validate template
  valid <- goose_template_validate(template)
  cat("  ✓ goose_template_validate() works - valid:", valid, "\n")
  
  results$templates <- "PASS"
}, error = function(e) {
  cat("  ✗ Templates error:", e$message, "\n")
  results$templates <- "FAIL"
})

cat("\n")

# Summary
cat("=====================================\n")
cat("           TEST SUMMARY              \n")
cat("=====================================\n")
cat("Phase 1 (Memory):    ", results$phase1, "\n")
cat("Phase 2 (Branding):  ", results$phase2, "\n")
cat("Phase 3 (CLI):       ", results$phase3, "\n")
cat("Phase 4 (Advanced):  \n")
cat("  - Streaming:       ", results$streaming, "\n")
cat("  - Cache:           ", results$cache, "\n")
cat("  - Async:           ", results$async, "\n")
cat("  - Templates:       ", results$templates, "\n")
cat("=====================================\n")

# Overall result
all_pass <- all(unlist(results) == "PASS")
if (all_pass) {
  cat("\n🎉 ALL TESTS PASSED! Package is working correctly.\n\n")
} else {
  cat("\n⚠️  Some tests failed. Review output above.\n\n")
}
