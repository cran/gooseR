# Enhanced goose_ask with automatic formatting
# This file provides an enhanced version of goose_ask with formatting enabled by default

#' Enhanced Ask Goose with Formatting
#'
#' Send a query to Goose AI and get a beautifully formatted response.
#' This is an enhanced version of goose_ask that formats the output by default.
#'
#' @param prompt Character string with the question or prompt
#' @param format Logical, whether to format the response (default TRUE)
#' @param output_format Character, either "text" or "json"
#' @param quiet Logical, suppress status messages
#' @param timeout Numeric, timeout in seconds (default 30)
#' @param session_id Optional session ID for context preservation
#' @param width Integer, line width for wrapping (default 80)
#' @param color Logical, whether to use color output (default TRUE)
#' @param ... Additional formatting arguments
#'
#' @return If format=TRUE and output_format="text", displays formatted response and returns raw text invisibly.
#'         If format=FALSE or output_format="json", returns the raw response.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a beautifully formatted response (default)
#' goose_ask("What is the tidyverse?")
#' 
#' # Get raw unformatted response
#' raw <- goose_ask("What is R?", format = FALSE)
#' 
#' # Get JSON response (never formatted)
#' data <- goose_ask("List 5 R packages", output_format = "json")
#' 
#' # Customize formatting
#' goose_ask("Explain ggplot2", width = 100, color = FALSE)
#' }
goose_ask <- function(prompt,
                     format = getOption("goose.auto_format", TRUE),
                     output_format = c("text", "json"),
                     quiet = TRUE,
                     timeout = 30,
                     session_id = NULL,
                     width = getOption("goose.format_width", 80),
                     color = getOption("goose.format_color", TRUE),
                     ...) {
  
  output_format <- match.arg(output_format)
  
  # Check configuration
  if (!goose_check_installation()) {
    stop("Goose CLI not found. Run goose_configure() first.")
  }
  
  # Build command arguments
  args <- c("run", "--text", shQuote(prompt))
  
  # Add output format
  args <- c(args, "--output-format", output_format)
  
  # Add quiet flag
  if (quiet) {
    args <- c(args, "--quiet")
  }
  
  # Add session if provided
  if (!is.null(session_id)) {
    args <- c(args, "--session-id", session_id)
  } else {
    args <- c(args, "--no-session")
  }
  
  # Execute command with timeout
  result <- tryCatch({
    system2("goose", 
            args = args,
            stdout = TRUE,
            stderr = TRUE,
            timeout = timeout)
  }, error = function(e) {
    if (grepl("timeout", e$message, ignore.case = TRUE)) {
      stop("Goose query timed out after ", timeout, " seconds")
    } else {
      stop("Goose CLI error: ", e$message)
    }
  })
  
  # Process response based on format
  if (output_format == "json") {
    # Try to parse JSON (never format JSON output)
    tryCatch({
      jsonlite::fromJSON(paste(result, collapse = "\n"))
    }, error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      result
    })
  } else {
    # Text response
    response <- paste(result, collapse = "\n")
    
    # Format if requested
    if (format) {
      format_ai_response(response, width = width, color = color, ...)
      # Return the raw response invisibly so it can be captured
      invisible(response)
    } else {
      response
    }
  }
}

#' Original goose_ask (unformatted)
#'
#' This is the original goose_ask function without formatting.
#' Use this if you need the raw behavior.
#'
#' @param prompt Character string with the question or prompt
#' @param output_format Character, either "text" or "json"
#' @param quiet Logical, suppress status messages
#' @param timeout Numeric, timeout in seconds (default 30)
#' @param session_id Optional session ID for context preservation
#'
#' @return Character string with response (text format) or list (json format)
#' @export
goose_ask_raw <- function(prompt, 
                         output_format = c("text", "json"),
                         quiet = TRUE,
                         timeout = 30,
                         session_id = NULL) {
  
  output_format <- match.arg(output_format)
  
  # Check configuration
  if (!goose_check_installation()) {
    stop("Goose CLI not found. Run goose_configure() first.")
  }
  
  # Build command arguments
  args <- c("run", "--text", shQuote(prompt))
  
  # Add output format
  args <- c(args, "--output-format", output_format)
  
  # Add quiet flag
  if (quiet) {
    args <- c(args, "--quiet")
  }
  
  # Add session if provided
  if (!is.null(session_id)) {
    args <- c(args, "--session-id", session_id)
  } else {
    args <- c(args, "--no-session")
  }
  
  # Execute command with timeout
  result <- tryCatch({
    system2("goose", 
            args = args,
            stdout = TRUE,
            stderr = TRUE,
            timeout = timeout)
  }, error = function(e) {
    if (grepl("timeout", e$message, ignore.case = TRUE)) {
      stop("Goose query timed out after ", timeout, " seconds")
    } else {
      stop("Goose CLI error: ", e$message)
    }
  })
  
  # Parse response based on format
  if (output_format == "json") {
    # Try to parse JSON
    tryCatch({
      jsonlite::fromJSON(paste(result, collapse = "\n"))
    }, error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      result
    })
  } else {
    # Return text response
    paste(result, collapse = "\n")
  }
}
