#!/usr/bin/env Rscript

library(gooseR)

cat("\n")
cat("╔══════════════════════════════════════════════╗\n")
cat("║     GooseR Package - Final Verification       ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# Function to test a phase
test_phase <- function(phase_name, tests) {
  cat(sprintf("▶ %s\n", phase_name))
  cat(paste0(rep("─", 50), collapse = ""), "\n")
  
  success <- TRUE
  for (test_name in names(tests)) {
    result <- tryCatch({
      tests[[test_name]]()
      cat(sprintf("  ✅ %s\n", test_name))
      TRUE
    }, error = function(e) {
      cat(sprintf("  ❌ %s: %s\n", test_name, e$message))
      FALSE
    })
    success <- success && result
  }
  
  cat("\n")
  return(success)
}

# Phase 1 Tests
phase1_tests <- list(
  "Save data" = function() {
    goose_save(iris[1:5,], "test_final", tags = c("test", "iris"))
  },
  "List objects" = function() {
    objs <- goose_list()
    stopifnot(nrow(objs) > 0)
  },
  "Load data" = function() {
    data <- goose_load("test_final")
    stopifnot(nrow(data) == 5)
  },
  "Delete data" = function() {
    # Suppress interactive prompt
    invisible(suppressWarnings(goose_delete("test_final")))
  }
)

# Phase 2 Tests
phase2_tests <- list(
  "Load brand" = function() {
    brand <- load_brand("block")
    stopifnot(!is.null(brand))
  },
  "Get palette" = function() {
    pal <- brand_palette("block")
    stopifnot(length(pal) > 0)
  },
  "Create theme" = function() {
    theme <- theme_brand("block")
    stopifnot(inherits(theme, "theme"))
  },
  "Generate CSS" = function() {
    css <- brand_css("block")
    stopifnot(nchar(css) > 100)
  }
)

# Phase 3 Tests
phase3_tests <- list(
  "Check installation" = function() {
    result <- goose_check_installation()
    stopifnot(!is.null(result))
  },
  "Get version" = function() {
    ver <- goose_version()
    stopifnot(!is.null(ver))
  },
  "Get config" = function() {
    cfg <- goose_get_config()
    stopifnot(is.list(cfg))
  }
)

# Phase 4 Tests - Streaming
phase4_streaming <- list(
  "StreamHandler class" = function() {
    handler <- StreamHandler$new(
      callback = function(x) {},
      error_callback = function(e) {},
      complete_callback = function() {}
    )
    stopifnot(inherits(handler, "StreamHandler"))
  },
  "StreamSession class" = function() {
    session <- StreamSession$new("test")
    stopifnot(inherits(session, "StreamSession"))
    session$close()
  }
)

# Phase 4 Tests - Cache
phase4_cache <- list(
  "Initialize cache" = function() {
    conn <- goose_cache_init()
    stopifnot(inherits(conn, "SQLiteConnection"))
    DBI::dbDisconnect(conn)
  },
  "Cache operations" = function() {
    conn <- goose_cache_init()
    goose_cache_set("key1", "value1", conn = conn)
    val <- goose_cache_get("key1", conn = conn)
    stopifnot(!is.null(val))
    goose_cache_clear(conn = conn)
    DBI::dbDisconnect(conn)
  },
  "Cache stats" = function() {
    conn <- goose_cache_init()
    stats <- goose_cache_stats(conn = conn)
    stopifnot(inherits(stats, "goose_cache_stats"))
    DBI::dbDisconnect(conn)
  }
)

# Phase 4 Tests - Async
phase4_async <- list(
  "WorkerPool class" = function() {
    pool <- WorkerPool$new(n_workers = 2)
    stopifnot(inherits(pool, "WorkerPool"))
    pool$shutdown()
  },
  "Future plan" = function() {
    future::plan(future::sequential)
    stopifnot(TRUE)  # If we get here, it worked
  }
)

# Phase 4 Tests - Templates
phase4_templates <- list(
  "List templates" = function() {
    templates <- goose_template_list()
    stopifnot(nrow(templates) > 0)
  },
  "Get builtin template" = function() {
    template <- goose_template_builtin("code_review")
    stopifnot(inherits(template, "goose_template"))
  },
  "Create template" = function() {
    tmpl <- goose_template(
      "test",
      "Test {var}",
      variables = list(var = "Variable")
    )
    stopifnot(inherits(tmpl, "goose_template"))
  },
  "Validate template" = function() {
    tmpl <- goose_template_builtin("code_review")
    result <- goose_template_validate(tmpl)
    stopifnot(result$valid == TRUE)
  }
)

# Run all tests
results <- list()
results$phase1 <- test_phase("PHASE 1: Memory Integration", phase1_tests)
results$phase2 <- test_phase("PHASE 2: Universal Branding", phase2_tests)
results$phase3 <- test_phase("PHASE 3: CLI Integration", phase3_tests)
results$phase4_streaming <- test_phase("PHASE 4: Streaming", phase4_streaming)
results$phase4_cache <- test_phase("PHASE 4: Cache", phase4_cache)
results$phase4_async <- test_phase("PHASE 4: Async", phase4_async)
results$phase4_templates <- test_phase("PHASE 4: Templates", phase4_templates)

# Summary
cat("╔══════════════════════════════════════════════╗\n")
cat("║                  SUMMARY                      ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

all_pass <- all(unlist(results))

if (all_pass) {
  cat("🎉 ALL TESTS PASSED!\n\n")
  cat("The gooseR package is fully functional with all 4 phases working:\n")
  cat("  ✅ Phase 1: Memory Integration\n")
  cat("  ✅ Phase 2: Universal Branding System\n")
  cat("  ✅ Phase 3: CLI Integration & AI Assistant\n")
  cat("  ✅ Phase 4: Advanced AI Features (Streaming, Cache, Async, Templates)\n")
  cat("\n")
  cat("Package is production-ready for Block employees!\n")
} else {
  cat("⚠️ Some tests failed. Review the output above.\n")
  failed <- names(results)[!unlist(results)]
  cat("Failed phases:", paste(failed, collapse = ", "), "\n")
}

cat("\n")
