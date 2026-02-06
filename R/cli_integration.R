# GooseR CLI Integration Module
# Phase 3: Direct Goose AI Integration
# Author: Brandon Theriault
# Date: December 2025

#' Configure Goose CLI Settings
#'
#' Set up Goose CLI configuration. Note: If Goose CLI is already configured
#' (e.g., for Block employees), this function is not needed. The package will
#' use the existing CLI configuration automatically.
#'
#' @param provider Character string specifying the AI provider (e.g., "openai", "anthropic")
#' @param model Character string specifying the model (e.g., "gpt-4", "claude-3")
#' @param api_key Character string with the API key (stored securely)
#' @param save_to_renviron Logical, whether to save to .Renviron file
#' @param check_cli_first Logical, check if CLI already works before configuring (default TRUE)
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # For Block employees with configured CLI, just check:
#' goose_test_cli()
#' 
#' # For external users who need API keys:
#' goose_configure(provider = "openai", model = "gpt-4", api_key = "your-key")
#' }
goose_configure <- function(provider = NULL, model = NULL, api_key = NULL, 
                           save_to_renviron = FALSE,
                           check_cli_first = TRUE) {
  
  # Check if Goose CLI is installed
  if (!goose_check_installation()) {
    stop("Goose CLI not found. Please install from: https://github.com/block/goose")
  }
  
  # Check if CLI already works (for Block employees)
  if (check_cli_first) {
    cli_works <- goose_test_cli(verbose = FALSE)
    if (cli_works) {
      message("[CLI] Goose CLI is already configured and working!")
      message("   No additional configuration needed.")
      return(invisible(TRUE))
    }
  }
  
  # Only configure if needed
  if (is.null(provider) && is.null(model) && is.null(api_key)) {
    message("[CLI] No configuration provided.")
    message("   If Goose CLI is already configured (e.g., Block employees),")
    message("   you can use gooseR without additional setup.")
    message("   ")
    message("   To test if CLI works: goose_test_cli()")
    message("   ")
    message("   For external users, provide API credentials:")
    message("   goose_configure(provider='openai', model='gpt-4', api_key='key')")
    return(invisible(FALSE))
  }
  
  # Set environment variables only if provided
  if (!is.null(provider)) {
    Sys.setenv(GOOSE_PROVIDER = provider)
  }
  
  if (!is.null(model)) {
    Sys.setenv(GOOSE_MODEL = model)
  }
  
  if (!is.null(api_key)) {
    Sys.setenv(GOOSE_API_KEY = api_key)
  }
  
  # Save to .Renviron if requested
  if (save_to_renviron && (!is.null(provider) || !is.null(model) || !is.null(api_key))) {
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
    
    # Read existing .Renviron
    if (file.exists(renviron_path)) {
      lines <- readLines(renviron_path)
    } else {
      lines <- character()
    }
    
    # Update or add variables
    vars_to_save <- list(
      GOOSE_PROVIDER = provider,
      GOOSE_MODEL = model,
      GOOSE_API_KEY = api_key
    )
    
    for (var_name in names(vars_to_save)) {
      if (!is.null(vars_to_save[[var_name]])) {
        # Remove existing line if present
        lines <- lines[!grepl(paste0("^", var_name, "="), lines)]
        # Add new line
        lines <- c(lines, paste0(var_name, "=", vars_to_save[[var_name]]))
      }
    }
    
    # Write back to .Renviron
    writeLines(lines, renviron_path)
    message("Configuration saved to .Renviron. Restart R session to apply.")
  }
  
  # Test if it works now
  cli_works <- goose_test_cli(verbose = TRUE)
  
  invisible(cli_works)
}

#' Get Current Goose Configuration
#'
#' @return List with provider, model, and api_key status
#' @export
goose_get_config <- function() {
  list(
    provider = Sys.getenv("GOOSE_PROVIDER", unset = NA),
    model = Sys.getenv("GOOSE_MODEL", unset = NA),
    api_key = Sys.getenv("GOOSE_API_KEY", unset = NA),
    cli_version = goose_version()
  )
}

#' Check Goose CLI Installation
#'
#' @return Logical, TRUE if Goose CLI is installed
#' @export
goose_check_installation <- function() {
  nzchar(Sys.which("goose"))
}

#' Get Goose CLI Version
#'
#' @return Character string with version or NULL if not installed
#' @export
goose_version <- function() {
  if (!goose_check_installation()) {
    return(NULL)
  }

  res <- .goose_cli_run(args = c("--version"), timeout = 10, retries = 0, quiet = TRUE)
  if (!identical(as.integer(res$status), 0L)) {
    return(NULL)
  }

  out <- paste(res$stdout, collapse = "\n")
  if (nchar(out) > 0) trimws(strsplit(out, "\n", fixed = TRUE)[[1]][1]) else NULL
}

#' Test if Goose CLI is Working
#'
#' Tests if Goose CLI is properly configured and can execute queries.
#' This is especially useful for Block employees who have CLI configured
#' but don't need to provide API keys in R.
#'
#' @param verbose Logical, whether to print status messages
#' @param timeout Numeric, timeout in seconds for the test query (default 60).
#'   Increase this if authentication prompts require more time.
#'
#' @return Logical, TRUE if CLI works, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if CLI works
#' if (goose_test_cli()) {
#'   # Ready to use goose_ask() etc.
#'   response <- goose_ask("Hello!")
#' } else {
#'   # May need configuration
#'   goose_configure(provider = "openai", model = "gpt-4", api_key = "key")
#' }
#' 
#' # Allow more time for authentication
#' goose_test_cli(timeout = 120)
#' }
goose_test_cli <- function(verbose = TRUE, timeout = 60) {
  
  # First check if CLI is installed
  if (!goose_check_installation()) {
    if (verbose) {
      message("[CLI] Goose CLI not installed")
      message("   Install from: https://github.com/block/goose")
    }
    return(FALSE)
  }
  
  if (verbose) {
    message("[CLI] Goose CLI found: ", goose_version())
  }
  
  # Try a simple test query
  test_query <- "Reply with just the word 'working' if you receive this."
  
  if (verbose) {
    message("[CLI] Testing CLI connection...")
    message("   (timeout: ", timeout, "s - if authentication is needed, enter credentials now)")
  }

  result <- tryCatch({
    res <- .goose_cli_run(
      args = c("run", "--text", test_query, "--no-session", "--quiet"),
      timeout = timeout,
      retries = 0,
      quiet = !isTRUE(verbose)
    )
    if (!identical(as.integer(res$status), 0L)) {
      return(NULL)
    }
    strsplit(paste(res$stdout, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  }, error = function(e) {
    return(NULL)
  }, warning = function(w) {
    return(NULL)
  })
  
  # Check if we got a response
  if (!is.null(result) && length(result) > 0) {
    # Check if response contains expected text or any reasonable response
    response_text <- paste(tolower(result), collapse = " ")
    
    # Look for signs of success
    success_indicators <- c("working", "yes", "received", "hello", "understand")
    
    # Check for error messages
    error_indicators <- c("error", "failed", "unauthorized", "api", "key", 
                         "credential", "authenticate", "token", "forbidden")
    
    has_success <- any(sapply(success_indicators, function(x) grepl(x, response_text)))
    has_error <- any(sapply(error_indicators, function(x) grepl(x, response_text)))
    
    if (has_success && !has_error) {
      if (verbose) {
        message("[CLI] Goose CLI is working properly!")
        message("   You can use all goose_* functions without additional configuration.")
      }
      return(TRUE)
    } else if (has_error) {
      if (verbose) {
        message("  Goose CLI needs configuration")
        message("   The CLI is installed but may need API credentials.")
        message("   ")
        message("   For Block employees:")
        message("   - Make sure you're logged into the Block Goose system")
        message("   - Check: goose session list")
        message("   ")
        message("   For external users:")
        message("   - Configure with: goose_configure(provider='openai', model='gpt-4', api_key='key')")
      }
      return(FALSE)
    } else {
      # Got a response but unclear if it's working
      if (verbose) {
        message("  Goose CLI response unclear")
        message("   Got response but couldn't verify if it's working properly.")
        message("   Try: goose_ask('Hello') to test manually.")
      }
      return(TRUE)  # Assume it's working if we got any response
    }
  } else {
    if (verbose) {
      message(" Goose CLI not responding")
      message("   The CLI is installed but not responding to queries.")
      message("   ")
      message("   Possible issues:")
      message("   - CLI needs configuration: run 'goose configure' in terminal")
      message("   - For Block employees: ensure you're on the corporate network")
      message("   - For external users: provide API credentials")
    }
    return(FALSE)
  }
}

# Note: goose_ask has been moved to goose_ask_enhanced.R
# It now includes formatting by default
# The raw version is available as goose_ask_raw()

#' Execute Goose Recipe
#'
#' Run a Goose recipe with parameters.
#'
#' @param recipe Character, recipe name or path to recipe file
#' @param params Named list of parameters to pass to recipe
#' @param explain Logical, show recipe explanation instead of running
#' @param render Logical, render recipe instead of running
#' @param timeout Numeric, timeout in seconds (default uses `getOption('goose.timeout', 300)`).
#'   Set to `Inf` for no timeout.
#' @param retries Integer, number of retries after the first attempt (default uses
#'   `getOption('goose.retries', 1)`). Retries only occur for timeout/transient errors.
#'
#' @return Recipe output or explanation
#' @export
goose_recipe <- function(recipe, params = list(), explain = FALSE, render = FALSE,
                        timeout = getOption("goose.timeout", 300),
                        retries = getOption("goose.retries", 1)) {
  
  if (!goose_check_installation()) {
    stop("Goose CLI not found")
  }
  
  # Build command
  args <- c("run", "--recipe", recipe)
  
  # Add parameters
  if (length(params) > 0) {
    for (name in names(params)) {
      args <- c(args, "--params", paste0(name, "=", params[[name]]))
    }
  }
  
  # Add flags
  if (explain) {
    args <- c(args, "--explain")
  }
  
  if (render) {
    args <- c(args, "--render-recipe")
  }
  
  res <- .goose_cli_run(args = args, timeout = timeout, retries = retries, quiet = TRUE)
  if (!identical(as.integer(res$status), 0L)) {
    stop("Goose CLI error (status=", res$status, "): ", paste(res$stderr, collapse = "\n"))
  }

  paste(res$stdout, collapse = "\n")
}

#' Create or Resume Goose Session
#'
#' Manage Goose sessions for maintaining context across queries.
#'
#' @param action Character, one of "create", "resume", "list", "remove"
#' @param name Optional session name
#' @param session_id Optional session ID
#' @param timeout Numeric, timeout in seconds (default uses `getOption('goose.timeout', 300)`).
#'   Set to `Inf` for no timeout.
#' @param retries Integer, number of retries after the first attempt (default uses
#'   `getOption('goose.retries', 1)`). Retries only occur for timeout/transient errors.
#'
#' @return Session information or query result
#' @export
goose_session <- function(action = c("create", "resume", "list", "remove"),
                         name = NULL,
                         session_id = NULL,
                         timeout = getOption("goose.timeout", 300),
                         retries = getOption("goose.retries", 1)) {
  
  action <- match.arg(action)
  
  if (!goose_check_installation()) {
    stop("Goose CLI not found")
  }
  
  if (action == "create") {
    # Create new session
    args <- c("session")
    if (!is.null(name)) {
      args <- c(args, "--name", name)
    }
    args <- c(args, "--no-interactive")  # Non-interactive mode
    
    res <- .goose_cli_run(args = args, timeout = timeout, retries = retries, quiet = TRUE)
    if (!identical(as.integer(res$status), 0L)) {
      stop("Goose CLI error (status=", res$status, "): ", paste(res$stderr, collapse = "\n"))
    }
    result <- strsplit(paste(res$stdout, collapse = "\n"), "\n", fixed = TRUE)[[1]]
    
    # Extract session ID from output
    session_info <- list(
      session_id = extract_session_id(result),
      name = name,
      created = Sys.time()
    )
    
    class(session_info) <- c("goose_session", "list")
    return(session_info)
    
  } else if (action == "list") {
    # List sessions
    res <- .goose_cli_run(args = c("session", "list"), timeout = timeout, retries = retries, quiet = TRUE)
    if (!identical(as.integer(res$status), 0L)) {
      stop("Goose CLI error (status=", res$status, "): ", paste(res$stderr, collapse = "\n"))
    }
    result <- strsplit(paste(res$stdout, collapse = "\n"), "\n", fixed = TRUE)[[1]]
    parse_session_list(result)
    
  } else if (action == "resume") {
    # Resume session
    if (is.null(session_id) && is.null(name)) {
      stop("Either session_id or name must be provided to resume")
    }
    
    args <- c("session", "--resume")
    if (!is.null(session_id)) {
      args <- c(args, "--session-id", session_id)
    } else if (!is.null(name)) {
      args <- c(args, "--name", name)
    }
    
    res <- .goose_cli_run(args = args, timeout = timeout, retries = retries, quiet = TRUE)
    if (!identical(as.integer(res$status), 0L)) {
      stop("Goose CLI error (status=", res$status, "): ", paste(res$stderr, collapse = "\n"))
    }
    message("Session resumed: ", session_id %||% name)
    invisible(TRUE)
    
  } else if (action == "remove") {
    # Remove session
    if (is.null(session_id) && is.null(name)) {
      stop("Either session_id or name must be provided to remove")
    }
    
    args <- c("session", "remove")
    if (!is.null(session_id)) {
      args <- c(args, session_id)
    }
    
    res <- .goose_cli_run(args = args, timeout = timeout, retries = retries, quiet = TRUE)
    if (!identical(as.integer(res$status), 0L)) {
      stop("Goose CLI error (status=", res$status, "): ", paste(res$stderr, collapse = "\n"))
    }
    message("Session removed")
    invisible(TRUE)
  }
}

#' Query Goose with Session Context
#'
#' Send a query to Goose while maintaining session context.
#'
#' @param prompt Character string with the query
#' @param session Goose session object or session ID
#' @param ... Additional arguments passed to goose_ask
#'
#' @return Query response
#' @export
goose_query <- function(prompt, session = NULL, ...) {
  
  # Extract session ID
  if (inherits(session, "goose_session")) {
    session_id <- session$session_id
  } else {
    session_id <- session
  }
  
  # Query with session
  goose_ask(prompt, session_id = session_id, ...)
}

# Helper functions ----

#' Extract session ID from Goose output
#' @keywords internal
extract_session_id <- function(output) {
  # Look for session ID pattern in output
  pattern <- "session[[:space:]]+([0-9]{8}_[0-9]{6})"
  matches <- regmatches(output, regexec(pattern, output, ignore.case = TRUE))
  
  for (match in matches) {
    if (length(match) > 1) {
      return(match[2])
    }
  }
  
  # Fallback: generate timestamp-based ID
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' Parse session list output
#' @keywords internal
parse_session_list <- function(output) {
  # Parse the session list output into a data frame
  # This will depend on the actual format of goose session list
  
  sessions <- data.frame(
    session_id = character(),
    name = character(),
    created = character(),
    last_used = character(),
    stringsAsFactors = FALSE
  )
  
  # TODO: Implement actual parsing based on goose output format
  
  sessions
}

#' Null coalescing operator
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Print method for goose_session
#' @param x goose_session object
#' @param ... additional arguments (unused)
#' @export
print.goose_session <- function(x, ...) {
  cat("Goose Session\n")
  cat("  ID:", x$session_id, "\n")
  if (!is.null(x$name)) {
    cat("  Name:", x$name, "\n")
  }
  cat("  Created:", format(x$created), "\n")
  invisible(x)
}
