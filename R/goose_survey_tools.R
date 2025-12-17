# goose_survey_tools.R - Smart survey data processing tools
# Tools for working with survey data, including intelligent column renaming

#' Intelligently Rename Survey Columns
#'
#' @description
#' Transforms long survey question text into short, meaningful variable names.
#' Combines janitor::clean_names() functionality with intelligent abbreviation
#' to create readable, consistent column names. Saves a mapping file for reference.
#'
#' @param data A data frame with columns to rename (typically survey data)
#' @param max_length Maximum length for new column names (default: 20)
#' @param style Naming style: "snake_case" (default), "camelCase", "SCREAMING_SNAKE"
#' @param abbreviate Logical, whether to use intelligent abbreviation (default: TRUE)
#' @param save_map Logical, whether to save the name mapping (default: TRUE)
#' @param map_file Path to save the mapping CSV file (auto-generated if NULL)
#' @param preview_only Logical, if TRUE only shows proposed changes without applying (default: FALSE)
#' @param custom_dict Named vector of custom abbreviations (e.g., c("satisfaction" = "sat"))
#'
#' @return 
#' If preview_only = FALSE: Data frame with renamed columns
#' If preview_only = TRUE: Data frame showing the mapping
#' 
#' @details
#' The function performs intelligent renaming by:
#' 1. Removing special characters and standardizing format (via clean_names)
#' 2. Detecting common survey patterns (scales, demographics, etc.)
#' 3. Creating meaningful abbreviations for long questions
#' 4. Ensuring uniqueness of all column names
#' 5. Saving a reference map for documentation
#'
#' Common patterns detected:
#' - Likert scales ("How satisfied...", "To what extent...")
#' - Demographics ("What is your age", "Gender", etc.)
#' - Yes/No questions
#' - Multiple choice questions
#' - Open-ended responses
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - rename survey columns
#' survey_clean <- goose_rename_columns(survey_data)
#' 
#' # Preview changes first
#' goose_rename_columns(survey_data, preview_only = TRUE)
#' 
#' # Use custom abbreviations
#' survey_clean <- goose_rename_columns(survey_data,
#'   custom_dict = c(
#'     "satisfaction" = "sat",
#'     "recommendation" = "rec",
#'     "likelihood" = "likely"
#'   )
#' )
#' 
#' # Use camelCase instead of snake_case
#' survey_clean <- goose_rename_columns(survey_data, style = "camelCase")
#' }
goose_rename_columns <- function(data,
                                max_length = 20,
                                style = c("snake_case", "camelCase", "SCREAMING_SNAKE"),
                                abbreviate = TRUE,
                                save_map = TRUE,
                                map_file = NULL,
                                preview_only = FALSE,
                                custom_dict = NULL) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame")
  }
  
  style <- match.arg(style)
  
  # Get original column names
  original_names <- names(data)
  n_cols <- length(original_names)
  
  cli::cli_h2("Intelligent Column Renaming")
  cli::cli_alert_info("Processing {n_cols} column{?s}")
  
  # Step 1: Basic cleaning with janitor (if available)
  if (requireNamespace("janitor", quietly = TRUE)) {
    # Map our style to janitor's case parameter
    janitor_case <- switch(style,
      "snake_case" = "snake",
      "camelCase" = "small_camel",
      "SCREAMING_SNAKE" = "screaming_snake",
      "snake"  # default
    )
    # Use janitor for initial cleaning
    clean_names <- janitor::make_clean_names(original_names, case = janitor_case)
  } else {
    # Basic cleaning if janitor not available
    clean_names <- original_names
    clean_names <- gsub("[^[:alnum:]_]", "_", clean_names)  # Replace special chars
    clean_names <- gsub("_{2,}", "_", clean_names)  # Remove multiple underscores
    clean_names <- gsub("^_|_$", "", clean_names)  # Remove leading/trailing underscores
    clean_names <- tolower(clean_names)
  }
  
  # Step 2: Intelligent abbreviation
  if (abbreviate) {
    new_names <- character(n_cols)
    
    for (i in seq_along(original_names)) {
      orig <- original_names[i]
      clean <- clean_names[i]
      
      # Check if already short enough
      if (nchar(clean) <= max_length) {
        new_names[i] <- clean
        next
      }
      
      # Apply intelligent abbreviation
      abbreviated <- abbreviate_survey_question(
        orig, 
        max_length = max_length,
        custom_dict = custom_dict
      )
      
      # Apply style
      if (style == "camelCase") {
        # Convert to camelCase
        parts <- strsplit(abbreviated, "_")[[1]]
        if (length(parts) > 1) {
          parts[-1] <- paste0(toupper(substring(parts[-1], 1, 1)), 
                              substring(parts[-1], 2))
          abbreviated <- paste0(parts, collapse = "")
        }
      } else if (style == "SCREAMING_SNAKE") {
        abbreviated <- toupper(abbreviated)
      }
      
      new_names[i] <- abbreviated
    }
  } else {
    new_names <- clean_names
  }
  
  # Step 3: Ensure uniqueness
  new_names <- make.unique(new_names, sep = "_")
  
  # Step 4: Create mapping
  name_map <- data.frame(
    original = original_names,
    cleaned = clean_names,
    final = new_names,
    length_reduction = nchar(original_names) - nchar(new_names),
    stringsAsFactors = FALSE
  )
  
  # Add abbreviation notes
  name_map$abbreviated <- ifelse(
    name_map$cleaned != name_map$final & abbreviate,
    "Yes",
    "No"
  )
  
  # Show preview or apply changes
  if (preview_only) {
    cli::cli_h3("Proposed Column Name Changes")
    
    # Show a nice table of changes
    changes <- name_map[name_map$original != name_map$final, ]
    if (nrow(changes) > 0) {
      for (i in seq_len(min(10, nrow(changes)))) {
        old <- substr(changes$original[i], 1, 50)
        if (nchar(changes$original[i]) > 50) old <- paste0(old, "...")
        
        cli::cli_alert_success(
          "{.field {old}} -> {.val {changes$final[i]}}"
        )
      }
      
      if (nrow(changes) > 10) {
        cli::cli_alert_info("... and {nrow(changes) - 10} more changes")
      }
    } else {
      cli::cli_alert_info("No changes needed - names are already clean")
    }
    
    return(name_map)
  }
  
  # Step 5: Apply the renaming
  names(data) <- new_names
  
  # Step 6: Save mapping if requested
  if (save_map) {
    if (is.null(map_file)) {
      map_file <- paste0("column_mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    }
    
    utils::write.csv(name_map, map_file, row.names = FALSE)
    cli::cli_alert_success("Column mapping saved to {.file {map_file}}")
    
    # Also save as an attribute
    attr(data, "column_map") <- name_map
  }
  
  # Summary
  n_changed <- sum(name_map$original != name_map$final)
  avg_reduction <- mean(name_map$length_reduction[name_map$length_reduction > 0])
  
  cli::cli_alert_success(
    "Renamed {n_changed} column{?s} (avg. length reduction: {round(avg_reduction, 1)} chars)"
  )
  
  # Show a few examples
  if (n_changed > 0) {
    examples <- head(name_map[name_map$original != name_map$final, ], 3)
    cli::cli_alert_info("Examples:")
    for (i in seq_len(nrow(examples))) {
      old_short <- substr(examples$original[i], 1, 40)
      if (nchar(examples$original[i]) > 40) old_short <- paste0(old_short, "...")
      cat("  ", old_short, " -> ", examples$final[i], "\n", sep = "")
    }
  }
  
  return(data)
}

#' Intelligent Abbreviation of Survey Questions
#'
#' @description
#' Internal function that intelligently abbreviates survey questions
#' into short, meaningful variable names.
#'
#' @param question The survey question text
#' @param max_length Maximum length for the abbreviated name
#' @param custom_dict Custom abbreviation dictionary
#'
#' @return Abbreviated variable name
#' @keywords internal
abbreviate_survey_question <- function(question, max_length = 20, custom_dict = NULL) {
  
  # Convert to lowercase for processing
  q_lower <- tolower(question)
  
  # Common survey patterns and their abbreviations
  # Order matters - more specific patterns first
  patterns <- list(
    # Demographics - very specific
    "what is your age" = "age",
    "how old are you" = "age",
    "what is your gender" = "gender",
    "highest level of education" = "edu",
    "education level" = "edu",
    "what is your annual household income" = "hh_income",
    "annual household income" = "hh_income",
    "household income" = "hh_income",
    "annual income" = "income",
    "employment status" = "employ",
    "marital status" = "marital",
    "zip code" = "zip",
    "postal code" = "postal",
    
    # NPS specific
    "how likely are you to recommend" = "nps",
    "would you recommend" = "nps",
    "net promoter" = "nps",
    "on a scale of 1-10, how likely" = "nps",
    
    # Satisfaction questions
    "how satisfied are you with" = "sat",
    "how satisfied" = "sat",
    "satisfaction with" = "sat",
    "satisfied" = "sat",
    
    # Agreement questions
    "to what extent do you agree" = "agree",
    "strongly agree" = "agree",
    "level of agreement" = "agree",
    
    # Frequency questions
    "how often do you" = "freq",
    "how often" = "freq",
    "how frequently" = "freq",
    "frequency" = "freq",
    
    # Importance questions
    "how important" = "import",
    "importance of" = "import",
    
    # Experience questions
    "years of experience" = "yrs_exp",
    "how long have you" = "duration",
    "length of time" = "duration",
    
    # Rating questions
    "rate your" = "rate",
    "rating of" = "rate",
    "on a scale" = "scale",
    
    # Yes/No questions
    "do you" = "yn",
    "have you" = "yn",
    "are you" = "yn",
    "is there" = "yn",
    
    # Open-ended
    "please describe your experience" = "desc_exp",
    "please describe" = "desc",
    "please explain" = "explain",
    "comments" = "comments",
    "additional feedback" = "feedback",
    "other" = "other"
  )
  
  # Add custom dictionary if provided
  if (!is.null(custom_dict)) {
    patterns <- c(custom_dict, patterns)
  }
  
  # Check for pattern matches
  result <- NULL
  for (pattern in names(patterns)) {
    if (grepl(pattern, q_lower, fixed = TRUE)) {
      base_name <- patterns[[pattern]]
      
      # Try to extract additional context
      # Remove the pattern and get key words
      remaining <- gsub(pattern, "", q_lower, fixed = TRUE)
      remaining <- trimws(remaining)
      
      # Extract key words (nouns, verbs)
      key_words <- extract_key_words(remaining, max_words = 2)
      
      if (length(key_words) > 0) {
        result <- paste(c(base_name, key_words), collapse = "_")
      } else {
        result <- base_name
      }
      break
    }
  }
  
  # If no pattern matched, use smart extraction
  if (is.null(result)) {
    result <- smart_extract(question, max_length)
  }
  
  # Ensure it fits within max_length
  if (nchar(result) > max_length) {
    result <- abbreviate_name(result, max_length)
  }
  
  # Clean up
  result <- gsub("[^[:alnum:]_]", "_", result)
  result <- gsub("_{2,}", "_", result)
  result <- gsub("^_|_$", "", result)
  result <- tolower(result)
  
  return(result)
}

#' Extract Key Words from Text
#' @keywords internal
extract_key_words <- function(text, max_words = 2) {
  # Remove common words
  stop_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", 
                  "for", "of", "with", "by", "from", "about", "into", "through",
                  "during", "before", "after", "above", "below", "between",
                  "your", "our", "their", "this", "that", "these", "those",
                  "what", "which", "who", "whom", "whose", "where", "when", "why", "how",
                  "please", "thank", "you")
  
  # Split into words
  words <- unlist(strsplit(text, "[^[:alnum:]]+"))
  words <- words[words != ""]
  words <- tolower(words)
  
  # Remove stop words
  words <- words[!words %in% stop_words]
  
  # Take first few meaningful words
  if (length(words) > max_words) {
    words <- words[1:max_words]
  }
  
  # Abbreviate long words
  words <- sapply(words, function(w) {
    if (nchar(w) > 6) {
      substr(w, 1, 4)
    } else {
      w
    }
  })
  
  return(words)
}

#' Smart Extract Variable Name from Question
#' @keywords internal
smart_extract <- function(question, max_length) {
  # Remove special characters and split
  words <- unlist(strsplit(question, "[^[:alnum:]]+"))
  words <- words[words != ""]
  
  # Remove very short words
  words <- words[nchar(words) > 2]
  
  # Take first few words
  if (length(words) > 3) {
    words <- words[1:3]
  }
  
  # Abbreviate each word
  abbrev <- sapply(words, function(w) {
    if (nchar(w) > 4) {
      substr(w, 1, 3)
    } else {
      w
    }
  })
  
  result <- paste(tolower(abbrev), collapse = "_")
  
  return(result)
}

#' Abbreviate a Name to Fit Length
#' @keywords internal
abbreviate_name <- function(name, max_length) {
  if (nchar(name) <= max_length) return(name)
  
  # Split by underscore
  parts <- strsplit(name, "_")[[1]]
  
  if (length(parts) == 1) {
    # Single word - just truncate
    return(substr(name, 1, max_length))
  }
  
  # Try to abbreviate each part
  while (nchar(paste(parts, collapse = "_")) > max_length && any(nchar(parts) > 2)) {
    # Find longest part
    longest_idx <- which.max(nchar(parts))
    # Shorten it by 1
    parts[longest_idx] <- substr(parts[longest_idx], 1, nchar(parts[longest_idx]) - 1)
  }
  
  result <- paste(parts, collapse = "_")
  
  # If still too long, truncate
  if (nchar(result) > max_length) {
    result <- substr(result, 1, max_length)
  }
  
  return(result)
}

#' View Column Name Mapping
#'
#' @description
#' Display or retrieve the column name mapping from a renamed data frame
#'
#' @param data A data frame that was processed with goose_rename_columns()
#' @param return_df Logical, if TRUE returns the mapping as a data frame (default: FALSE)
#'
#' @return 
#' If return_df = TRUE: Returns the mapping data frame
#' If return_df = FALSE: Invisibly returns NULL and displays the mapping
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # View the mapping
#' goose_view_column_map(survey_clean)
#' 
#' # Get mapping as data frame
#' map_df <- goose_view_column_map(survey_clean, return_df = TRUE)
#' }
goose_view_column_map <- function(data, return_df = FALSE) {
  
  # Check for mapping attribute
  map <- attr(data, "column_map")
  
  if (is.null(map)) {
    cli::cli_alert_warning("No column mapping found. Was this data processed with goose_rename_columns()?")
    return(invisible(NULL))
  }
  
  if (return_df) {
    return(map)
  }
  
  # Display the mapping nicely
  cli::cli_h2("Column Name Mapping")
  
  n_changed <- sum(map$original != map$final)
  cli::cli_alert_info("Total columns: {nrow(map)}, Changed: {n_changed}")
  
  if (n_changed > 0) {
    changed <- map[map$original != map$final, ]
    
    cli::cli_h3("Changed Columns")
    for (i in seq_len(min(20, nrow(changed)))) {
      old <- changed$original[i]
      if (nchar(old) > 60) {
        old <- paste0(substr(old, 1, 57), "...")
      }
      
      cat(sprintf("  %2d. %-60s -> %s\n", i, old, changed$final[i]))
    }
    
    if (nrow(changed) > 20) {
      cli::cli_alert_info("... and {nrow(changed) - 20} more")
    }
  }
  
  # Summary stats
  if (any(map$abbreviated == "Yes")) {
    avg_reduction <- mean(map$length_reduction[map$abbreviated == "Yes"])
    cli::cli_alert_success(
      "Average length reduction for abbreviated columns: {round(avg_reduction, 1)} characters"
    )
  }
  
  invisible(NULL)
}
