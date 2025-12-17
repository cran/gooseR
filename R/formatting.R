# formatting.R - Text formatting utilities for gooseR AI responses

#' Format AI response text for better display in R console
#'
#' @param text Character string with AI response
#' @param width Integer, line width for wrapping (default 80)
#' @param indent Integer, spaces to indent wrapped lines (default 0)
#' @param preserve_code Logical, whether to preserve code blocks (default TRUE)
#' @param color Logical, whether to use color output (default TRUE)
#' 
#' @return Formatted text (invisibly), printed to console
#' @export
format_ai_response <- function(text, width = 80, indent = 0, 
                               preserve_code = TRUE, color = TRUE) {
  if (is.null(text) || length(text) == 0) return(invisible(NULL))
  
  # Clean up escaped characters
  text <- gsub("\\\\n", "\n", text)
  text <- gsub("\\\\t", "\t", text)
  text <- gsub('\\\\"', '"', text)
  text <- gsub("\\\\'", "'", text)
  
  # Split into lines
  lines <- strsplit(text, "\n")[[1]]
  
  # Process each line
  formatted_lines <- character()
  in_code_block <- FALSE
  code_block <- character()
  
  for (line in lines) {
    # Check for code blocks
    if (preserve_code && grepl("^```", line)) {
      if (in_code_block) {
        # End of code block - format and add
        in_code_block <- FALSE
        if (color) {
          cli::cli_div(theme = list(
            ".code" = list(color = "cyan"),
            ".code-block" = list(
              "background-color" = "#f0f0f0",
              "padding" = "4px",
              "margin" = "4px 0"
            )
          ))
          cli::cli_code(code_block)
          cli::cli_end()
        } else {
          cat("```\n")
          cat(paste(code_block, collapse = "\n"), "\n")
          cat("```\n")
        }
        code_block <- character()
      } else {
        # Start of code block
        in_code_block <- TRUE
        if (!color) cat(line, "\n")
      }
      next
    }
    
    if (in_code_block) {
      code_block <- c(code_block, line)
      next
    }
    
    # Format headers
    if (grepl("^#{1,6} ", line)) {
      if (color) {
        level <- nchar(gsub("^(#{1,6}) .*", "\\1", line))
        header_text <- gsub("^#{1,6} ", "", line)
        
        if (level == 1) {
          cli::cli_h1(header_text)
        } else if (level == 2) {
          cli::cli_h2(header_text)
        } else if (level == 3) {
          cli::cli_h3(header_text)
        } else {
          cli::cli_text(cli::style_bold(header_text))
        }
      } else {
        cat("\n", line, "\n", sep = "")
        cat(rep("=", nchar(line)), "\n", sep = "")
      }
      next
    }
    
    # Format bullet points
    if (grepl("^[*+-] ", line)) {
      bullet_text <- gsub("^[*+-] ", "", line)
      if (color) {
        cli::cli_li(bullet_text)
      } else {
        cat("  - ", bullet_text, "\n", sep = "")
      }
      next
    }
    
    # Format numbered lists
    if (grepl("^[0-9]+\\. ", line)) {
      list_text <- gsub("^[0-9]+\\. ", "", line)
      if (color) {
        cli::cli_li(list_text)
      } else {
        num <- gsub("^([0-9]+)\\. .*", "\\1", line)
        cat("  ", num, ". ", list_text, "\n", sep = "")
      }
      next
    }
    
    # Bold text
    if (grepl("\\*\\*.*\\*\\*", line)) {
      if (color) {
        # Replace **text** with bold
        line <- gsub("\\*\\*([^*]+)\\*\\*", 
                    cli::style_bold("\\1"), 
                    line)
      } else {
        line <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", line)
      }
    }
    
    # Italic text
    if (grepl("\\*[^*]+\\*", line)) {
      if (color) {
        line <- gsub("\\*([^*]+)\\*", 
                    cli::style_italic("\\1"), 
                    line)
      } else {
        line <- gsub("\\*([^*]+)\\*", "\\1", line)
      }
    }
    
    # Inline code
    if (grepl("`[^`]+`", line)) {
      if (color) {
        line <- gsub("`([^`]+)`", 
                    cli::col_cyan("\\1"), 
                    line)
      } else {
        line <- gsub("`([^`]+)`", "'\\1'", line)
      }
    }
    
    # Wrap long lines
    if (nchar(line) > width) {
      wrapped <- strwrap(line, width = width, indent = indent, 
                        exdent = indent + 2)
      for (w in wrapped) {
        if (color) {
          cli::cli_text(w)
        } else {
          cat(w, "\n")
        }
      }
    } else {
      if (line == "") {
        cat("\n")
      } else if (color) {
        cli::cli_text(line)
      } else {
        cat(line, "\n")
      }
    }
  }
  
  invisible(text)
}

#' Print formatted AI response
#'
#' @param x AI response object or text
#' @param ... Additional arguments passed to format_ai_response
#'
#' @export
print.goose_response <- function(x, ...) {
  format_ai_response(x, ...)
}

# Note: goose_ask and goose_review_code formatting is now integrated
# directly into those functions. See goose_ask_enhanced.R

#' Get copy-friendly version of formatted text
#'
#' Removes formatting artifacts and returns clean text for copying
#'
#' @param text Character string with formatted text
#' @param preserve_markdown Logical, keep markdown formatting (default TRUE)
#'
#' @return Clean text suitable for copying
#' @export
#'
#' @examples
#' \dontrun{
#' response <- goose_ask("What is R?")
#' # Copy-friendly version:
#' clean <- goose_clean_text(response)
#' cat(clean)
#' }
goose_clean_text <- function(text, preserve_markdown = TRUE) {
  if (is.null(text) || length(text) == 0) return("")
  
  # Remove any ANSI color codes
  text <- gsub("\033\\[[0-9;]*m", "", text)
  
  # Remove excessive line breaks
  text <- gsub("\n\n\n+", "\n\n", text)
  
  # Remove cli formatting artifacts
  text <- gsub("^[--]+\\s*", "", text, perl = TRUE)
  
  if (!preserve_markdown) {
    # Remove markdown if requested
    text <- gsub("#{1,6}\\s+", "", text)  # Headers
    text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text)  # Bold
    text <- gsub("\\*([^*]+)\\*", "\\1", text)  # Italic
    # Split into lines to handle line-start patterns
    lines <- strsplit(text, "\n")[[1]]
    lines <- gsub("^[*+-]\\s+", "", lines)  # Bullets
    lines <- gsub("^\\d+\\.\\s+", "", lines)  # Numbers
    text <- paste(lines, collapse = "\n")
    text <- gsub("`([^`]+)`", "\\1", text)  # Inline code
  }
  
  text
}

#' Set global formatting options for gooseR
#'
#' @param auto_format Logical, automatically format all AI responses
#' @param width Line width for text wrapping
#' @param color Use colored output
#' @param code_highlight Highlight code blocks
#'
#' @export
#' @examples
#' \dontrun{
#' # Enable auto-formatting for all AI responses
#' goose_format_options(auto_format = TRUE, width = 100)
#' 
#' # Disable colors for plain text output
#' goose_format_options(color = FALSE)
#' }
goose_format_options <- function(auto_format = TRUE, width = 80, 
                                 color = TRUE, code_highlight = TRUE) {
  options(
    goose.auto_format = auto_format,
    goose.format_width = width,
    goose.format_color = color,
    goose.code_highlight = code_highlight
  )
  
  if (auto_format) {
    cli::cli_alert_success("Auto-formatting enabled for gooseR responses")
  }
  
  invisible(list(
    auto_format = auto_format,
    width = width,
    color = color,
    code_highlight = code_highlight
  ))
}

#' Create a markdown-style divider
#' @param char Character to use for divider
#' @param width Width of divider
#' @export
goose_divider <- function(char = "-", width = 60) {
  if (getOption("goose.format_color", TRUE)) {
    cli::cli_rule()
  } else {
    cat(rep(char, width), "\n", sep = "")
  }
}

#' Format and display a gooseR conversation
#'
#' @param messages List of messages (question/response pairs)
#' @param show_dividers Logical, show dividers between messages
#'
#' @export
format_conversation <- function(messages, show_dividers = TRUE) {
  for (i in seq_along(messages)) {
    msg <- messages[[i]]
    
    if (!is.null(msg$question)) {
      cli::cli_div(theme = list(span.question = list(color = "blue")))
      cli::cli_text("{.question Q: {msg$question}}")
      cli::cli_end()
    }
    
    if (!is.null(msg$response)) {
      format_ai_response(msg$response)
    }
    
    if (show_dividers && i < length(messages)) {
      goose_divider()
    }
  }
}


