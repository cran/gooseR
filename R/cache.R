#' Response Caching Module for GooseR
#'
#' Provides intelligent caching of AI responses for performance
#' @name goose_cache
#' @importFrom DBI dbConnect dbExecute dbGetQuery dbDisconnect
#' @seealso Requires Suggests: RSQLite. Functions in this module check for RSQLite availability at runtime.
#' @importFrom digest digest
#' @importFrom jsonlite toJSON fromJSON write_json read_json
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils write.csv read.csv
NULL

#' Initialize Cache Database
#'
#' Create or connect to the cache database
#' @param cache_dir Directory for cache database (default: tempdir() for CRAN compliance)
#' @return DBI connection object
#' @export
#' @examples
#' \dontrun{
#' # Initialize cache in temp directory (CRAN compliant)
#' conn <- goose_cache_init()
#' 
#' # Or specify custom directory
#' conn <- goose_cache_init(cache_dir = tempdir())
#' }
goose_cache_init <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    # Use tempdir() by default for CRAN compliance
    cache_dir <- file.path(tempdir(), "gooseR_cache")
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  db_path <- file.path(cache_dir, "goose_cache.db")
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite is required for caching. Please install it: install.packages('RSQLite')")
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  # Create cache table if not exists
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS cache (
      hash TEXT PRIMARY KEY,
      query TEXT NOT NULL,
      response TEXT NOT NULL,
      model TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      access_count INTEGER DEFAULT 1,
      last_accessed DATETIME DEFAULT CURRENT_TIMESTAMP,
      metadata TEXT
    )
  ")
  
  # Create index for faster lookups
  DBI::dbExecute(conn, "
    CREATE INDEX IF NOT EXISTS idx_timestamp ON cache(timestamp)
  ")
  
  DBI::dbExecute(conn, "
    CREATE INDEX IF NOT EXISTS idx_access_count ON cache(access_count)
  ")
  
  return(conn)
}

#' Cache an AI Response
#'
#' Store a query-response pair in the cache
#' @param query The query text
#' @param response The response text
#' @param model Optional model identifier
#' @param metadata Optional metadata as list
#' @param conn Database connection (auto-created if NULL)
#' @return Logical indicating success
#' @export
goose_cache_set <- function(query, response, model = NULL, 
                           metadata = NULL, conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  # Generate hash for query
  hash <- digest::digest(paste(query, model), algo = "sha256")
  
  # Serialize metadata
  meta_json <- NA_character_
  if (!is.null(metadata)) {
    meta_json <- as.character(jsonlite::toJSON(metadata, auto_unbox = TRUE))
  }
  
  # Insert or update cache entry
  result <- tryCatch({
    # First check if entry exists
    existing <- DBI::dbGetQuery(conn, 
      "SELECT access_count FROM cache WHERE hash = ?", 
      params = list(hash))
    
    if (nrow(existing) > 0) {
      # Update existing entry
      DBI::dbExecute(conn, "
        UPDATE cache 
        SET query = ?, response = ?, model = ?, metadata = ?,
            timestamp = CURRENT_TIMESTAMP, 
            access_count = access_count + 1,
            last_accessed = CURRENT_TIMESTAMP
        WHERE hash = ?
      ", params = list(query, response, 
                      ifelse(is.null(model), NA_character_, model), 
                      ifelse(is.na(meta_json), NA_character_, meta_json), 
                      hash))
    } else {
      # Insert new entry
      DBI::dbExecute(conn, "
        INSERT INTO cache 
        (hash, query, response, model, metadata, timestamp, access_count, last_accessed)
        VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP, 1, CURRENT_TIMESTAMP)
      ", params = list(hash, query, response, 
                      ifelse(is.null(model), NA_character_, model), 
                      ifelse(is.na(meta_json), NA_character_, meta_json)))
    }
    TRUE
  }, error = function(e) {
    warning("Cache set failed: ", e$message)
    FALSE
  })
  
  if (close_conn) DBI::dbDisconnect(conn)
  return(result)
}

#' Get Cached Response
#'
#' Retrieve a cached response for a query
#' @param query The query text
#' @param model Optional model identifier
#' @param max_age Maximum age in seconds (NULL for no limit)
#' @param conn Database connection
#' @return Response text or NULL if not found
#' @export
goose_cache_get <- function(query, model = NULL, max_age = NULL, conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  # Generate hash for query
  hash <- digest::digest(paste(query, model), algo = "sha256")
  
  # Build query with optional age filter
  sql <- "SELECT response FROM cache WHERE hash = ?"
  params <- list(hash)
  
  if (!is.null(max_age)) {
    sql <- paste0(sql, " AND datetime(timestamp, '+", max_age, " seconds') > datetime('now')")
  }
  
  result <- DBI::dbGetQuery(conn, sql, params = params)
  
  response <- NULL
  if (nrow(result) > 0) {
    response <- result$response[1]
    
    # Update access count and timestamp
    DBI::dbExecute(conn, "
      UPDATE cache 
      SET access_count = access_count + 1,
          last_accessed = CURRENT_TIMESTAMP
      WHERE hash = ?
    ", params = list(hash))
  }
  
  if (close_conn) DBI::dbDisconnect(conn)
  return(response)
}

#' Clear Cache
#'
#' Clear cache entries based on criteria
#' @param older_than Clear entries older than this (seconds)
#' @param pattern Clear entries matching this pattern
#' @param all Clear all entries
#' @param conn Database connection
#' @return Number of entries cleared
#' @export
goose_cache_clear <- function(older_than = NULL, pattern = NULL, 
                             all = FALSE, conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  if (all) {
    result <- DBI::dbExecute(conn, "DELETE FROM cache")
  } else if (!is.null(older_than)) {
    result <- DBI::dbExecute(conn, "
      DELETE FROM cache 
      WHERE datetime(timestamp, '+? seconds') < datetime('now')
    ", params = list(older_than))
  } else if (!is.null(pattern)) {
    result <- DBI::dbExecute(conn, "
      DELETE FROM cache 
      WHERE query LIKE ?
    ", params = list(paste0("%", pattern, "%")))
  } else {
    result <- 0
  }
  
  if (close_conn) DBI::dbDisconnect(conn)
  message("Cleared ", result, " cache entries")
  return(result)
}

#' Get Cache Statistics
#'
#' Get statistics about cache usage
#' @param conn Database connection
#' @return List of cache statistics
#' @export
goose_cache_stats <- function(conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  stats <- list()
  
  # Total entries
  stats$total_entries <- DBI::dbGetQuery(conn, 
    "SELECT COUNT(*) as count FROM cache")$count
  
  # Total size
  stats$total_size <- DBI::dbGetQuery(conn,
    "SELECT SUM(LENGTH(query) + LENGTH(response)) as size FROM cache")$size
  
  # Average response size
  stats$avg_response_size <- DBI::dbGetQuery(conn,
    "SELECT AVG(LENGTH(response)) as size FROM cache")$size
  
  # Most accessed
  stats$most_accessed <- DBI::dbGetQuery(conn,
    "SELECT query, access_count 
     FROM cache 
     ORDER BY access_count DESC 
     LIMIT 10")
  
  # Recent entries
  stats$recent_entries <- DBI::dbGetQuery(conn,
    "SELECT query, timestamp 
     FROM cache 
     ORDER BY timestamp DESC 
     LIMIT 10")
  
  # Cache hit rate (if tracking)
  stats$total_accesses <- DBI::dbGetQuery(conn,
    "SELECT SUM(access_count) as total FROM cache")$total
  
  # Age distribution
  stats$age_distribution <- DBI::dbGetQuery(conn,
    "SELECT 
      CASE 
        WHEN julianday('now') - julianday(timestamp) < 1 THEN '< 1 day'
        WHEN julianday('now') - julianday(timestamp) < 7 THEN '1-7 days'
        WHEN julianday('now') - julianday(timestamp) < 30 THEN '7-30 days'
        ELSE '> 30 days'
      END as age_group,
      COUNT(*) as count
     FROM cache
     GROUP BY age_group")
  
  if (close_conn) DBI::dbDisconnect(conn)
  
  class(stats) <- c("goose_cache_stats", "list")
  return(stats)
}

#' Print Cache Statistics
#'
#' @param x Cache statistics object
#' @param ... Additional arguments
#' @export
print.goose_cache_stats <- function(x, ...) {
  cat("GooseR Cache Statistics\n")
  cat("=======================\n")
  cat("Total entries:", x$total_entries, "\n")
  cat("Total size:", format(x$total_size, big.mark = ","), "bytes\n")
  cat("Avg response size:", round(x$avg_response_size), "bytes\n")
  cat("Total accesses:", x$total_accesses, "\n")
  
  if (nrow(x$most_accessed) > 0) {
    cat("\nMost Accessed Queries:\n")
    for (i in 1:min(5, nrow(x$most_accessed))) {
      cat(sprintf("  %d. %s (accessed %d times)\n",
                  i,
                  substr(x$most_accessed$query[i], 1, 50),
                  x$most_accessed$access_count[i]))
    }
  }
  
  if (nrow(x$age_distribution) > 0) {
    cat("\nCache Age Distribution:\n")
    print(x$age_distribution, row.names = FALSE)
  }
}

#' Cached Query Execution
#'
#' Execute query with automatic caching
#' @param query The query to execute
#' @param use_cache Whether to use cache
#' @param max_age Maximum cache age in seconds
#' @param force_refresh Force new execution
#' @return Query response
#' @export
goose_cached <- function(query, use_cache = TRUE, max_age = 86400, 
                        force_refresh = FALSE) {
  
  # Check cache first
  if (use_cache && !force_refresh) {
    cached <- goose_cache_get(query, max_age = max_age)
    if (!is.null(cached)) {
      message("[Using cached response]")
      return(cached)
    }
  }
  
  # Execute query
  message("[Executing fresh query]")
  response <- goose_ask(query)  # Use the existing goose_ask function
  
  # Cache the response
  if (use_cache) {
    goose_cache_set(query, response)
  }
  
  return(response)
}

#' Cache Warmup
#'
#' Pre-populate cache with common queries
#' @param queries Vector of queries to cache
#' @param parallel Execute in parallel
#' @return Number of queries cached
#' @export
goose_cache_warmup <- function(queries, parallel = FALSE) {
  cached <- 0
  
  if (parallel && requireNamespace("future", quietly = TRUE)) {
    # Parallel execution
    future::plan(future::multisession, workers = 4)
    
    results <- future.apply::future_lapply(queries, function(q) {
      response <- goose_ask(q)
      goose_cache_set(q, response)
      return(TRUE)
    })
    
    cached <- sum(unlist(results))
    future::plan(future::sequential)
  } else {
    # Sequential execution
    for (query in queries) {
      message("Caching: ", substr(query, 1, 50), "...")
      response <- goose_ask(query)
      if (goose_cache_set(query, response)) {
        cached <- cached + 1
      }
    }
  }
  
  message("Cached ", cached, " queries")
  return(cached)
}

#' Export Cache
#'
#' Export cache to file for backup or sharing
#' @param file Path to export file
#' @param format Export format (json, csv, rds)
#' @param conn Database connection
#' @return Number of entries exported
#' @export
goose_cache_export <- function(file, format = "json", conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  # Get all cache entries
  data <- DBI::dbGetQuery(conn, "SELECT * FROM cache")
  
  if (nrow(data) == 0) {
    warning("No cache entries to export")
    return(0)
  }
  
  # Export based on format
  if (format == "json") {
    jsonlite::write_json(data, file, pretty = TRUE)
  } else if (format == "csv") {
    write.csv(data, file, row.names = FALSE)
  } else if (format == "rds") {
    saveRDS(data, file)
  } else {
    stop("Unsupported format: ", format)
  }
  
  if (close_conn) DBI::dbDisconnect(conn)
  
  message("Exported ", nrow(data), " cache entries to ", file)
  return(nrow(data))
}

#' Import Cache
#'
#' Import cache from file
#' @param file Path to import file
#' @param format Import format (json, csv, rds)
#' @param conn Database connection
#' @return Number of entries imported
#' @export
goose_cache_import <- function(file, format = "json", conn = NULL) {
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- goose_cache_init()
    close_conn <- TRUE
  }
  
  # Import based on format
  if (format == "json") {
    data <- jsonlite::read_json(file, simplifyVector = TRUE)
  } else if (format == "csv") {
    data <- read.csv(file, stringsAsFactors = FALSE)
  } else if (format == "rds") {
    data <- readRDS(file)
  } else {
    stop("Unsupported format: ", format)
  }
  
  # Import entries
  imported <- 0
  for (i in 1:nrow(data)) {
    result <- goose_cache_set(
      query = data$query[i],
      response = data$response[i],
      model = data$model[i],
      metadata = if (!is.null(data$metadata[i])) 
        jsonlite::fromJSON(data$metadata[i]) else NULL
    )
    if (result) imported <- imported + 1
  }
  
  if (close_conn) DBI::dbDisconnect(conn)
  
  message("Imported ", imported, " cache entries from ", file)
  return(imported)
}
