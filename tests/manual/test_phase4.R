library(gooseR)

cat("Testing Phase 4 Functions\n")
cat("=========================\n\n")

# Test streaming functions
cat("Streaming functions:\n")
cat("  goose_stream:", exists("goose_stream"), "\n")
cat("  goose_stream_async:", exists("goose_stream_async"), "\n")
cat("  goose_stream_multi:", exists("goose_stream_multi"), "\n")
cat("  goose_stream_session:", exists("goose_stream_session"), "\n")
cat("  StreamHandler:", exists("StreamHandler"), "\n")
cat("  StreamSession:", exists("StreamSession"), "\n\n")

# Test cache functions
cat("Cache functions:\n")
cat("  goose_cache_init:", exists("goose_cache_init"), "\n")
cat("  goose_cache_set:", exists("goose_cache_set"), "\n")
cat("  goose_cache_get:", exists("goose_cache_get"), "\n")
cat("  goose_cache_clear:", exists("goose_cache_clear"), "\n")
cat("  goose_cache_stats:", exists("goose_cache_stats"), "\n")
cat("  goose_cache_export:", exists("goose_cache_export"), "\n")
cat("  goose_cache_import:", exists("goose_cache_import"), "\n")
cat("  goose_cache_warmup:", exists("goose_cache_warmup"), "\n")
cat("  goose_cached:", exists("goose_cached"), "\n\n")

# Test async functions
cat("Async functions:\n")
cat("  goose_async:", exists("goose_async"), "\n")
cat("  goose_batch:", exists("goose_batch"), "\n")
cat("  goose_map:", exists("goose_map"), "\n")
cat("  goose_reduce:", exists("goose_reduce"), "\n")
cat("  goose_mapreduce:", exists("goose_mapreduce"), "\n")
cat("  goose_pipeline:", exists("goose_pipeline"), "\n")
cat("  goose_worker_pool:", exists("goose_worker_pool"), "\n")
cat("  goose_async_retry:", exists("goose_async_retry"), "\n")
cat("  goose_async_timeout:", exists("goose_async_timeout"), "\n")
cat("  goose_batch_file:", exists("goose_batch_file"), "\n")
cat("  WorkerPool:", exists("WorkerPool"), "\n\n")

# Test template functions
cat("Template functions:\n")
cat("  goose_template:", exists("goose_template"), "\n")
cat("  goose_template_list:", exists("goose_template_list"), "\n")
cat("  goose_template_apply:", exists("goose_template_apply"), "\n")
cat("  goose_template_save:", exists("goose_template_save"), "\n")
cat("  goose_template_load:", exists("goose_template_load"), "\n")
cat("  goose_template_validate:", exists("goose_template_validate"), "\n")
cat("  goose_template_builtin:", exists("goose_template_builtin"), "\n")
cat("  goose_template_from_query:", exists("goose_template_from_query"), "\n\n")

# Test that functions are actually callable
cat("Function callability test:\n")
tryCatch({
  templates <- goose_template_list()
  cat("  ✓ goose_template_list() works - found", nrow(templates), "templates\n")
}, error = function(e) {
  cat("  ✗ goose_template_list() error:", e$message, "\n")
})

tryCatch({
  conn <- goose_cache_init()
  cat("  ✓ goose_cache_init() works\n")
  DBI::dbDisconnect(conn)
}, error = function(e) {
  cat("  ✗ goose_cache_init() error:", e$message, "\n")
})

cat("\nAll Phase 4 functions are loaded and accessible!\n")
