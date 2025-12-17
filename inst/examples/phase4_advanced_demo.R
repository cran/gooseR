#!/usr/bin/env Rscript
#' GooseR Phase 4: Advanced AI Features Demo
#' 
#' Demonstrates streaming, caching, async execution, and templates
#' @author Brandon Theriault
#' @date 2025-12-02

# Load required libraries
library(devtools)
devtools::load_all()  # Load development version
library(future)
library(promises)

cat("========================================\n")
cat("GooseR Phase 4: Advanced AI Features Demo\n")
cat("========================================\n\n")

# ============================================================================
# 1. STREAMING RESPONSES
# ============================================================================
cat("1. STREAMING RESPONSES\n")
cat("----------------------\n")

# Note: Streaming requires Goose CLI to support --stream flag
# This is a simulation of how it would work

cat("Simulating streaming response...\n")
tryCatch({
  # Simple streaming
  goose_stream("Write a haiku about R programming")
  
  # Custom callback
  goose_stream(
    "Explain data science in 3 sentences",
    callback = function(chunk) {
      cat("[CHUNK] ", chunk, "\n")
    },
    complete_callback = function() {
      cat("\n[STREAMING COMPLETE]\n")
    }
  )
}, error = function(e) {
  cat("Note: Streaming requires Goose CLI --stream support\n")
  cat("Simulation complete\n")
})

cat("\n")

# ============================================================================
# 2. RESPONSE CACHING
# ============================================================================
cat("2. RESPONSE CACHING\n")
cat("-------------------\n")

# Initialize cache
cache_conn <- goose_cache_init()

# First query - will execute
cat("First query (fresh execution):\n")
response1 <- goose_cached("What is the capital of France?", use_cache = TRUE)
cat("Response:", substr(response1, 1, 50), "...\n\n")

# Second query - should use cache
cat("Second query (from cache):\n")
response2 <- goose_cached("What is the capital of France?", use_cache = TRUE)
cat("Response:", substr(response2, 1, 50), "...\n\n")

# Cache statistics
stats <- goose_cache_stats(cache_conn)
print(stats)

# Clear old cache entries
goose_cache_clear(older_than = 86400, conn = cache_conn)  # Clear >1 day old

DBI::dbDisconnect(cache_conn)
cat("\n")

# ============================================================================
# 3. ASYNC/PARALLEL EXECUTION
# ============================================================================
cat("3. ASYNC/PARALLEL EXECUTION\n")
cat("---------------------------\n")

# Batch processing
queries <- c(
  "What is machine learning?",
  "Explain neural networks",
  "What is deep learning?"
)

cat("Processing", length(queries), "queries in parallel...\n")
results <- goose_batch(queries, max_workers = 3, progress = TRUE)

cat("\nResults summary:\n")
for (i in seq_along(results)) {
  cat(sprintf("Query %d: %d characters\n", i, nchar(results[[i]])))
}

# Map-reduce pattern
cat("\nMap-Reduce Example:\n")
code_snippets <- c(
  "function(x) x^2",
  "for(i in 1:10) print(i)",
  "data.frame(x = 1:5, y = 6:10)"
)

cat("Analyzing", length(code_snippets), "code snippets...\n")
reviews <- goose_map(
  code_snippets,
  "Review this R code briefly: {x}",
  max_workers = 2
)

cat("Summarizing results...\n")
summary <- goose_reduce(reviews, "Summarize these code reviews in 2 sentences:")
cat("Summary:", substr(summary, 1, 100), "...\n\n")

# ============================================================================
# 4. PROMPT TEMPLATES
# ============================================================================
cat("4. PROMPT TEMPLATES\n")
cat("-------------------\n")

# Create custom template
code_review_template <- goose_template(
  name = "code_review",
  template = "Review this {language} code:\n```{language}\n{code}\n```\nFocus: {focus}",
  description = "Code review template",
  variables = list(
    language = "Programming language",
    code = "Code to review",
    focus = "Areas to focus on"
  )
)

# Save template
goose_template_save(code_review_template, overwrite = TRUE)

# Use template
cat("Using code review template:\n")
review_result <- goose_template_apply(
  "code_review",
  language = "R",
  code = "mean(c(1, 2, NA, 4), na.rm = TRUE)",
  focus = "handling of missing values"
)
cat("Review:", substr(review_result, 1, 100), "...\n\n")

# List available templates
cat("Available templates:\n")
templates <- goose_template_list()
print(head(templates, 5))
cat("\n")

# Use built-in template
cat("Using built-in template 'explain_concept':\n")
explanation <- goose_template_apply(
  goose_template_builtin("explain_concept"),
  concept = "recursion",
  background = "beginner programming",
  complexity = "simple",
  examples_count = "2",
  focus = "practical applications"
)
cat("Explanation:", substr(explanation, 1, 100), "...\n\n")

# ============================================================================
# 5. WORKER POOL
# ============================================================================
cat("5. WORKER POOL\n")
cat("--------------\n")

# Create worker pool
pool <- goose_worker_pool(n_workers = 2)

# Add queries to queue
pool$add("What is R?", id = "r_intro")
pool$add("What is Python?", id = "python_intro")
pool$add("Compare R and Python", id = "comparison")

# Process queue
cat("Processing worker pool queue...\n")
pool_results <- pool$process()

cat("Pool results:\n")
for (name in names(pool_results)) {
  cat(sprintf("- %s: %d characters\n", name, nchar(pool_results[[name]])))
}

# Shutdown pool
pool$shutdown()
cat("\n")

# ============================================================================
# 6. ADVANCED CACHING
# ============================================================================
cat("6. ADVANCED CACHING\n")
cat("-------------------\n")

# Cache warmup with common queries
common_queries <- c(
  "How to read CSV in R?",
  "How to create a plot in R?",
  "How to handle missing data in R?"
)

cat("Warming up cache with", length(common_queries), "common queries...\n")
cached_count <- goose_cache_warmup(common_queries, parallel = FALSE)
cat("Cached", cached_count, "queries\n\n")

# Export cache for backup
cache_file <- "goose_cache_backup.json"
cat("Exporting cache to", cache_file, "...\n")
exported <- goose_cache_export(cache_file, format = "json")
cat("Exported", exported, "entries\n\n")

# Clean up
if (file.exists(cache_file)) {
  file.remove(cache_file)
}

# ============================================================================
# 7. PERFORMANCE METRICS
# ============================================================================
cat("7. PERFORMANCE METRICS\n")
cat("----------------------\n")

# Measure query performance
cat("Benchmarking query performance...\n")

# Time a single query
start_time <- Sys.time()
result <- goose_ask("What is 2+2?")
end_time <- Sys.time()
single_time <- as.numeric(end_time - start_time, units = "secs")

cat(sprintf("Single query time: %.2f seconds\n", single_time))

# Time cached query
start_time <- Sys.time()
result <- goose_cached("What is 2+2?")
end_time <- Sys.time()
cached_time <- as.numeric(end_time - start_time, units = "secs")

cat(sprintf("Cached query time: %.2f seconds\n", cached_time))
cat(sprintf("Cache speedup: %.1fx\n", single_time / cached_time))

cat("\n========================================\n")
cat("Phase 4 Demo Complete!\n")
cat("========================================\n")

# Summary
cat("\nPhase 4 Features Demonstrated:\n")
cat("✅ Streaming responses (simulated)\n")
cat("✅ Response caching with SQLite\n")
cat("✅ Parallel batch processing\n")
cat("✅ Map-reduce patterns\n")
cat("✅ Prompt templates (custom & built-in)\n")
cat("✅ Worker pools for queue processing\n")
cat("✅ Cache warmup and export\n")
cat("✅ Performance benchmarking\n")

cat("\nNext Steps:\n")
cat("- Implement IDE addins for RStudio/Positron\n")
cat("- Add Quarto/RMarkdown integration\n")
cat("- Create conversation export/import\n")
cat("- Build comprehensive benchmark suite\n")
cat("- Add more built-in templates\n")
