#' Streaming Response Module for GooseR
#'
#' Provides real-time streaming responses from Goose AI
#' @name goose_streaming
#' @importFrom processx process
#' @importFrom jsonlite fromJSON
#' @importFrom R6 R6Class
#' @importFrom promises promise promise_race as.promise
#' @importFrom later later
NULL

#' Stream Response from Goose
#'
#' Execute a query with streaming response handling
#' @param query The query to send to Goose
#' @param callback Function to call with each chunk (default: cat)
#' @param error_callback Function to call on error
#' @param complete_callback Function to call on completion
#' @param session_id Optional session ID for context
#' @param max_time Numeric, maximum runtime in seconds for the stream (default uses
#'   `getOption('goose.stream_timeout', Inf)`). Set to `Inf` for no limit.
#' @param idle_timeout Numeric, maximum time in seconds without receiving any output
#'   before aborting the stream (default uses `getOption('goose.stream_idle_timeout', Inf)`).
#'   Set to `Inf` for no limit.
#' @return Invisible NULL (results handled via callbacks)
#' @export
#' @examples
#' \dontrun{
#' # Simple streaming with default output
#' goose_stream("Explain R functions")
#' 
#' # Custom callback for processing chunks
#' goose_stream("Write a function", callback = function(chunk) {
#'   message("Received: ", nchar(chunk), " characters")
#'   cat(chunk)
#' })
#' }
goose_stream <- function(query, 
                        callback = NULL,
                        error_callback = NULL,
                        complete_callback = NULL,
                        session_id = NULL,
                        max_time = getOption("goose.stream_timeout", Inf),
                        idle_timeout = getOption("goose.stream_idle_timeout", Inf)) {
  
  # Default callbacks
  if (is.null(callback)) {
    callback <- function(chunk) cat(chunk)
  }
  if (is.null(error_callback)) {
    error_callback <- function(err) warning("Stream error: ", err)
  }
  if (is.null(complete_callback)) {
    complete_callback <- function() message("\n[Stream complete]")
  }
  
  # Create stream handler
  handler <- StreamHandler$new(
    callback = callback,
    error_callback = error_callback,
    complete_callback = complete_callback
  )
  
  # Start streaming process
  handler$start(query, session_id, max_time = max_time, idle_timeout = idle_timeout)
  
  invisible(NULL)
}

#' Stream Handler R6 Class
#'
#' Manages streaming responses from Goose
#' @export
StreamHandler <- R6::R6Class(
  "StreamHandler",
  public = list(
    #' @field process The processx process object
    process = NULL,
    #' @field buffer Accumulated response buffer
    buffer = "",
    #' @field callback Chunk callback function
    callback = NULL,
    #' @field error_callback Error callback function
    error_callback = NULL,
    #' @field complete_callback Completion callback function
    complete_callback = NULL,
    #' @field max_time Maximum runtime in seconds (Inf = no limit)
    max_time = Inf,
    #' @field idle_timeout Maximum time in seconds without output (Inf = no limit)
    idle_timeout = Inf,
    
    #' Initialize stream handler
    #' @param callback Function to call with chunks
    #' @param error_callback Function for errors
    #' @param complete_callback Function for completion
    initialize = function(callback, error_callback, complete_callback) {
      self$callback <- callback
      self$error_callback <- error_callback
      self$complete_callback <- complete_callback
    },
    
    #' Start streaming process
    #' @param query The query to execute
    #' @param session_id Optional session ID
    #' @param max_time Numeric, maximum runtime in seconds (Inf = no limit)
    #' @param idle_timeout Numeric, maximum time in seconds without output (Inf = no limit)
    start = function(query, session_id = NULL, max_time = Inf, idle_timeout = Inf) {
      self$max_time <- max_time
      self$idle_timeout <- idle_timeout

      # Build command with streaming flag
      cmd_args <- c("--stream", "--query", query)
      if (!is.null(session_id)) {
        cmd_args <- c(cmd_args, "--session", session_id)
      }
      
      # Start process with stdout callback
      self$process <- processx::process$new(
        command = "goose",
        args = cmd_args,
        stdout = "|",
        stderr = "|",
        supervise = TRUE
      )
      
      # Start monitoring
      self$monitor()
    },
    
    #' Monitor streaming process
    #' @description Internal loop that reads stdout/stderr, parses chunks, and enforces
    #'   `max_time` / `idle_timeout`.
    monitor = function() {
      start_time <- Sys.time()
      last_output_time <- Sys.time()

      while (self$process$is_alive()) {
        now <- Sys.time()

        if (is.finite(self$max_time) && as.numeric(difftime(now, start_time, units = "secs")) > self$max_time) {
          self$process$kill()
          self$error_callback(paste0("Stream exceeded max_time=", self$max_time, " seconds"))
          break
        }

        if (is.finite(self$idle_timeout) && as.numeric(difftime(now, last_output_time, units = "secs")) > self$idle_timeout) {
          self$process$kill()
          self$error_callback(paste0("Stream exceeded idle_timeout=", self$idle_timeout, " seconds"))
          break
        }

        # Check for output (no timeout parameter needed)
        output <- self$process$read_output_lines()
        
        if (length(output) > 0) {
          last_output_time <- Sys.time()
          for (line in output) {
            # Parse streaming JSON chunks
            tryCatch({
              chunk <- jsonlite::fromJSON(line)
              if (!is.null(chunk$content)) {
                self$callback(chunk$content)
                self$buffer <- paste0(self$buffer, chunk$content)
              }
            }, error = function(e) {
              # Handle non-JSON output
              self$callback(line)
              self$buffer <- paste0(self$buffer, line, "\n")
            })
          }
        }
        
        # Check for errors
        errors <- self$process$read_error_lines()
        if (length(errors) > 0) {
          self$error_callback(paste(errors, collapse = "\n"))
        }
        
        # Small delay to prevent CPU spinning
        Sys.sleep(0.1)
      }
      
      # Process complete
      self$complete_callback()
    },
    
    #' Get accumulated buffer
    #' @return Complete response text
    get_response = function() {
      return(self$buffer)
    },
    
    #' Stop streaming
    #' @description Kill the underlying streaming process if it is still running.
    stop = function() {
      if (!is.null(self$process) && self$process$is_alive()) {
        self$process$kill()
      }
    }
  )
)

#' Async Stream with Promise
#'
#' Stream response that returns a promise for async handling
#' @param query The query to execute
#' @param show_progress Show progress during streaming
#' @return A promise that resolves to the complete response
#' @export
#' @examples
#' \dontrun{
#' library(promises)
#' goose_stream_async("Explain promises") %...>%
#'   { cat("Complete response:", .) }
#' }
goose_stream_async <- function(query, show_progress = TRUE) {
  promises::promise(function(resolve, reject) {
    buffer <- ""
    
    goose_stream(
      query = query,
      callback = function(chunk) {
        if (show_progress) cat(chunk)
        buffer <<- paste0(buffer, chunk)
      },
      error_callback = function(err) {
        reject(err)
      },
      complete_callback = function() {
        resolve(buffer)
      }
    )
  })
}

#' Stream Multiple Queries
#'
#' Stream multiple queries sequentially with progress
#' @param queries Vector of queries to execute
#' @param callback Optional callback for each response
#' @return List of responses
#' @export
goose_stream_multi <- function(queries, callback = NULL) {
  responses <- list()
  
  for (i in seq_along(queries)) {
    message(sprintf("\n[Query %d/%d]: %s", i, length(queries), 
                   substr(queries[i], 1, 50)))
    
    buffer <- ""
    goose_stream(
      query = queries[i],
      callback = function(chunk) {
        cat(chunk)
        buffer <<- paste0(buffer, chunk)
      },
      complete_callback = function() {
        responses[[i]] <<- buffer
        if (!is.null(callback)) {
          callback(i, buffer)
        }
      }
    )
    
    # Wait for completion
    Sys.sleep(0.5)
  }
  
  return(responses)
}

#' Create Streaming Session
#'
#' Create a persistent streaming session for multiple queries
#' @param session_name Name for the session
#' @return StreamSession object
#' @export
goose_stream_session <- function(session_name = NULL) {
  if (is.null(session_name)) {
    session_name <- paste0("stream_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  StreamSession$new(session_name)
}

#' Stream Session R6 Class
#'
#' Manages a persistent streaming session
#' @export
StreamSession <- R6::R6Class(
  "StreamSession",
  public = list(
    #' @field session_id Unique session identifier
    session_id = NULL,
    #' @field history Query/response history
    history = list(),
    #' @field active Whether session is active
    active = TRUE,
    
    #' Initialize session
    #' @param session_id Session identifier
    initialize = function(session_id) {
      self$session_id <- session_id
      message("Stream session created: ", session_id)
    },
    
    #' Send query in session
    #' @param query Query to execute
    #' @param show_output Show streaming output
    #' @return Response text
    query = function(query, show_output = TRUE) {
      if (!self$active) {
        stop("Session is not active")
      }
      
      buffer <- ""
      goose_stream(
        query = query,
        session_id = self$session_id,
        callback = function(chunk) {
          if (show_output) cat(chunk)
          buffer <<- paste0(buffer, chunk)
        }
      )
      
      # Store in history
      self$history <- append(self$history, list(
        list(query = query, response = buffer, timestamp = Sys.time())
      ))
      
      return(invisible(buffer))
    },
    
    #' Get session history
    #' @return Data frame of queries and responses
    get_history = function() {
      if (length(self$history) == 0) {
        return(data.frame(query = character(), 
                         response = character(), 
                         timestamp = character()))
      }
      
      do.call(rbind, lapply(self$history, as.data.frame))
    },
    
    #' Close session
    #' @description Mark session inactive (client-side). This does not remove any
    #'   server-side Goose session.
    #' @return Invisible NULL
    close = function() {
      self$active <- FALSE
      message("Stream session closed: ", self$session_id)
    }
  )
)
