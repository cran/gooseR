#' Async and Parallel Execution Module for GooseR
#'
#' Provides asynchronous and parallel query execution
#' @name goose_async
#' @importFrom future future plan multisession sequential
#' @importFrom promises promise as.promise promise_race
#' @importFrom future.apply future_lapply
#' @importFrom later later
#' @importFrom utils txtProgressBar setTxtProgressBar
NULL

#' Execute Query Asynchronously
#'
#' Run a query in the background and return a promise
#' @param query The query to execute
#' @param session_id Optional session ID
#' @return A promise that resolves to the response
#' @export
#' @examples
#' \dontrun{
#' library(promises)
#' 
#' # Single async query
#' goose_async("Explain async programming") %...>%
#'   { cat("Response:", .) }
#' 
#' # Chain multiple async operations
#' goose_async("Write a function") %...>%
#'   { goose_async(paste("Optimize this:", .)) } %...>%
#'   { cat("Optimized:", .) }
#' }
goose_async <- function(query, session_id = NULL) {
  future::future({
    goose_ask(query, session_id = session_id)
  }) %>% promises::as.promise()
}

#' Execute Multiple Queries in Parallel
#'
#' Run multiple queries simultaneously
#' @param queries Vector or list of queries
#' @param max_workers Maximum number of parallel workers
#' @param progress Show progress bar
#' @param cache Use caching for responses
#' @return List of responses
#' @export
#' @examples
#' \dontrun{
#' queries <- c(
#'   "Explain R functions",
#'   "Write a data analysis script",
#'   "Create a visualization"
#' )
#' 
#' results <- goose_batch(queries, max_workers = 3)
#' }
goose_batch <- function(queries, max_workers = 4, progress = TRUE, cache = TRUE) {
  
  # Setup parallel backend
  original_plan <- future::plan()
  future::plan(future::multisession, workers = min(max_workers, length(queries)))
  on.exit(future::plan(original_plan))
  
  # Progress setup
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(queries), style = 3)
    on.exit(close(pb), add = TRUE)
  }
  
  # Execute queries in parallel
  results <- future.apply::future_lapply(
    seq_along(queries),
    function(i) {
      query <- queries[i]
      
      # Check cache first
      if (cache) {
        cached <- goose_cache_get(query)
        if (!is.null(cached)) {
          if (progress) setTxtProgressBar(pb, i)
          return(cached)
        }
      }
      
      # Execute query
      response <- tryCatch({
        goose_ask(query)
      }, error = function(e) {
        paste("Error:", e$message)
      })
      
      # Cache response
      if (cache && !grepl("^Error:", response)) {
        goose_cache_set(query, response)
      }
      
      if (progress) setTxtProgressBar(pb, i)
      return(response)
    },
    future.seed = TRUE
  )
  
  # Add names if queries are named
  if (!is.null(names(queries))) {
    names(results) <- names(queries)
  }
  
  return(results)
}

#' Create Async Query Pipeline
#'
#' Chain multiple async operations together
#' @param ... Query functions or strings
#' @return A promise chain
#' @export
#' @examples
#' \dontrun{
#' goose_pipeline(
#'   "Write a data analysis function",
#'   ~ paste("Add error handling to:", .),
#'   ~ paste("Add documentation to:", .),
#'   ~ paste("Create tests for:", .)
#' ) %...>% { cat("Final result:", .) }
#' }
goose_pipeline <- function(...) {
  steps <- list(...)
  
  # Start with first step
  if (is.character(steps[[1]])) {
    result <- goose_async(steps[[1]])
  } else if (is.function(steps[[1]])) {
    result <- promises::promise(function(resolve, reject) {
      resolve(steps[[1]]())
    })
  }
  
  # Chain remaining steps
  for (i in 2:length(steps)) {
    step <- steps[[i]]
    if (is.character(step)) {
      result <- result %...>% { goose_async(step) }
    } else if (is.function(step)) {
      result <- result %...>% step %...>% { goose_async(.) }
    }
  }
  
  return(result)
}

#' Map Async Function Over Data
#'
#' Apply an AI query to each element of a dataset
#' @param data Vector or list of data elements
#' @param query_template Query template with \{x\} placeholder
#' @param max_workers Maximum parallel workers
#' @return List of AI responses
#' @export
#' @examples
#' \dontrun{
#' # Analyze multiple code snippets
#' code_snippets <- c("function(x) x^2", "for(i in 1:10) print(i)")
#' reviews <- goose_map(code_snippets, "Review this R code: {x}")
#' }
goose_map <- function(data, query_template, max_workers = 4) {
  # Create queries from template
  queries <- sapply(data, function(x) {
    gsub("\\{x\\}", x, query_template)
  })
  
  # Execute in parallel
  goose_batch(queries, max_workers = max_workers)
}

#' Reduce Results with AI
#'
#' Combine multiple results using AI
#' @param results List of results to combine
#' @param reduce_prompt Prompt for reduction
#' @return Combined result
#' @export
goose_reduce <- function(results, reduce_prompt = "Summarize these results:") {
  combined <- paste(results, collapse = "\n\n---\n\n")
  query <- paste(reduce_prompt, "\n\n", combined)
  goose_ask(query)
}

#' Parallel Map-Reduce with AI
#'
#' Map-reduce pattern for AI processing
#' @param data Input data
#' @param map_query Query template for mapping
#' @param reduce_query Query for reduction
#' @param max_workers Maximum parallel workers
#' @return Reduced result
#' @export
goose_mapreduce <- function(data, map_query, reduce_query, max_workers = 4) {
  # Map phase
  mapped <- goose_map(data, map_query, max_workers)
  
  # Reduce phase
  goose_reduce(mapped, reduce_query)
}

#' Async Query with Timeout
#'
#' Execute query with timeout protection
#' @param query The query to execute
#' @param timeout Timeout in seconds (default from goose.timeout option, or 300)
#' @return Response or timeout error
#' @export
goose_async_timeout <- function(query, timeout = getOption("goose.timeout", 300)) {
  future::future({
    goose_ask(query)
  }, globals = TRUE) %>%
    promises::promise_race(
      promises::promise(function(resolve, reject) {
        later::later(function() {
          reject("Query timeout")
        }, timeout)
      })
    )
}

#' Batch Process File
#'
#' Process a file of queries in parallel
#' @param file Path to file with queries (one per line)
#' @param output_file Optional output file for results
#' @param max_workers Maximum parallel workers
#' @return List of results
#' @export
goose_batch_file <- function(file, output_file = NULL, max_workers = 4) {
  # Read queries from file
  queries <- readLines(file)
  queries <- queries[nchar(queries) > 0]  # Remove empty lines
  
  message("Processing ", length(queries), " queries from ", file)
  
  # Process in parallel
  results <- goose_batch(queries, max_workers = max_workers)
  
  # Save results if output file specified
  if (!is.null(output_file)) {
    output <- mapply(function(q, r) {
      paste0("Query: ", q, "\n\nResponse:\n", r, "\n\n", 
             paste(rep("=", 80), collapse = ""), "\n")
    }, queries, results, SIMPLIFY = TRUE)
    
    writeLines(output, output_file)
    message("Results saved to ", output_file)
  }
  
  return(results)
}

#' Create Async Worker Pool
#'
#' Create a pool of workers for processing queries
#' @param n_workers Number of workers
#' @return WorkerPool object
#' @export
goose_worker_pool <- function(n_workers = 4) {
  WorkerPool$new(n_workers)
}

#' Worker Pool R6 Class
#'
#' Manages a pool of async workers
#' @export
WorkerPool <- R6::R6Class(
  "WorkerPool",
  public = list(
    #' @field n_workers Number of workers
    n_workers = NULL,
    #' @field queue Query queue
    queue = NULL,
    #' @field results Results list
    results = NULL,
    #' @field active Whether pool is active
    active = TRUE,
    
    #' Initialize worker pool
    #' @param n_workers Number of workers
    initialize = function(n_workers = 4) {
      self$n_workers <- n_workers
      self$queue <- list()
      self$results <- list()
      future::plan(future::multisession, workers = n_workers)
      message("Worker pool created with ", n_workers, " workers")
    },
    
    #' Add query to queue
    #' @param query Query to add
    #' @param id Optional query ID
    add = function(query, id = NULL) {
      if (is.null(id)) {
        id <- paste0("query_", length(self$queue) + 1)
      }
      self$queue[[id]] <- query
      invisible(self)
    },
    
    #' Process all queued queries
    #' @param progress Show progress
    #' @return List of results
    process = function(progress = TRUE) {
      if (length(self$queue) == 0) {
        message("No queries in queue")
        return(list())
      }
      
      message("Processing ", length(self$queue), " queries...")
      
      # Process queue in parallel
      self$results <- future.apply::future_lapply(
        self$queue,
        function(q) goose_ask(q),
        future.seed = TRUE
      )
      
      names(self$results) <- names(self$queue)
      
      # Clear queue
      self$queue <- list()
      
      return(self$results)
    },
    
    #' Get results
    #' @return Results list
    get_results = function() {
      return(self$results)
    },
    
    #' Clear queue
    #' @description Remove all queued items without processing them.
    #' @return Invisible self
    clear_queue = function() {
      self$queue <- list()
      message("Queue cleared")
    },

    #' Shutdown pool
    #' @description Shut down the worker pool and reset the future plan to sequential.
    #' @return Invisible self
    shutdown = function() {
      future::plan(future::sequential)
      self$active <- FALSE
      message("Worker pool shutdown")
    }
  )
)

#' Async Query with Retry
#'
#' Execute query with automatic retry on failure
#' @param query The query to execute
#' @param max_retries Maximum number of retries
#' @param retry_delay Delay between retries in seconds
#' @return Response or error
#' @export
goose_async_retry <- function(query, max_retries = 3, retry_delay = 2) {
  attempt <- 0
  
  repeat {
    attempt <- attempt + 1
    
    result <- tryCatch({
      goose_ask(query)
    }, error = function(e) {
      if (attempt < max_retries) {
        message("Attempt ", attempt, " failed. Retrying in ", retry_delay, " seconds...")
        Sys.sleep(retry_delay)
        NULL
      } else {
        stop("Query failed after ", max_retries, " attempts: ", e$message)
      }
    })
    
    if (!is.null(result)) {
      return(result)
    }
    
    if (attempt >= max_retries) {
      break
    }
  }
}
