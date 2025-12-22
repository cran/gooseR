# memory_bulk.R - Bulk memory management functions for gooseR
# Priority implementation for v0.1.1

#' Clear all items in a category
#'
#' @param category Character string specifying the category to clear
#' @param confirm Logical, whether to ask for confirmation (default TRUE)
#' @param verbose Logical, whether to print progress messages
#'
#' @return Invisible integer count of deleted items
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all items in "temp" category
#' goose_clear_category("temp")
#' 
#' # Clear without confirmation
#' goose_clear_category("temp", confirm = FALSE)
#' }
goose_clear_category <- function(category, confirm = TRUE, verbose = TRUE) {
  items <- goose_list(category = category)
  
  if (nrow(items) == 0) {
    if (verbose) cli::cli_alert_info("No items found in category '{category}'")
    return(invisible(0))
  }
  
  if (confirm) {
    cli::cli_alert_warning(
      "About to delete {nrow(items)} item{?s} from category '{category}'"
    )
    response <- readline("Type 'yes' to confirm: ")
    if (tolower(response) != "yes") {
      cli::cli_alert_info("Cancelled - no items deleted")
      return(invisible(0))
    }
  }
  
  count <- 0
  cli::cli_progress_bar("Deleting items", total = nrow(items))
  
  for (i in seq_len(nrow(items))) {
    tryCatch({
      goose_delete(name = items$name[i], category = category, confirm = FALSE)
      count <- count + 1
      cli::cli_progress_update()
    }, error = function(e) {
      cli::cli_alert_danger("Failed to delete '{items$name[i]}': {e$message}")
    })
  }
  
  cli::cli_progress_done()
  
  if (verbose) {
    cli::cli_alert_success("Deleted {count} item{?s} from category '{category}'")
  }
  
  invisible(count)
}

#' Clear all items with specified tags
#'
#' @param tags Character vector of tags
#' @param confirm Logical, whether to ask for confirmation (default TRUE)
#' @param verbose Logical, whether to print progress messages
#'
#' @return Invisible integer count of deleted items
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all items tagged as "test"
#' goose_clear_tags("test")
#' 
#' # Clear multiple tags
#' goose_clear_tags(c("test", "temp", "draft"))
#' }
goose_clear_tags <- function(tags, confirm = TRUE, verbose = TRUE) {
  # Get all items with any of the specified tags
  all_items <- data.frame()
  for (tag in tags) {
    items <- tryCatch({
      goose_list(tags = tag)
    }, error = function(e) {
      data.frame()
    })
    
    if (!is.null(items) && nrow(items) > 0) {
      all_items <- rbind(all_items, items)
    }
  }
  
  # Remove duplicates (items with multiple matching tags)
  if (nrow(all_items) > 0) {
    all_items <- all_items[!duplicated(paste(all_items$name, all_items$category)), ]
  }
  
  if (nrow(all_items) == 0) {
    if (verbose) {
      cli::cli_alert_info("No items found with tag{?s}: {paste(tags, collapse = ', ')}")
    }
    return(invisible(0))
  }
  
  if (confirm) {
    cli::cli_alert_warning(
      "About to delete {nrow(all_items)} item{?s} with tag{?s}: {paste(tags, collapse = ', ')}"
    )
    response <- readline("Type 'yes' to confirm: ")
    if (tolower(response) != "yes") {
      cli::cli_alert_info("Cancelled - no items deleted")
      return(invisible(0))
    }
  }
  
  count <- 0
  cli::cli_progress_bar("Deleting items", total = nrow(all_items))
  
  for (i in seq_len(nrow(all_items))) {
    tryCatch({
      goose_delete(
        name = all_items$name[i], 
        category = all_items$category[i],
        confirm = FALSE  # Pass through the confirm setting
      )
      count <- count + 1
      cli::cli_progress_update()
    }, error = function(e) {
      cli::cli_alert_danger(
        "Failed to delete '{all_items$name[i]}': {e$message}"
      )
    })
  }
  
  cli::cli_progress_done()
  
  if (verbose) {
    cli::cli_alert_success("Deleted {count} item{?s} with specified tags")
  }
  
  invisible(count)
}

#' Clear all gooseR memory
#'
#' @param confirm Logical, whether to ask for confirmation (default TRUE)
#' @param backup_first Logical, whether to create backup before clearing
#' @param backup_dir Character, directory for backup if backup_first is TRUE
#'
#' @return Invisible integer count of deleted items
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all memory with confirmation
#' goose_clear_all()
#' 
#' # Clear with backup
#' goose_clear_all(backup_first = TRUE)
#' }
goose_clear_all <- function(confirm = TRUE, backup_first = FALSE, 
                            backup_dir = "goose_backup") {
  items <- goose_list()
  
  if (nrow(items) == 0) {
    cli::cli_alert_info("No items in gooseR memory")
    return(invisible(0))
  }
  
  if (backup_first) {
    cli::cli_alert_info("Creating backup...")
    backup_count <- goose_backup(backup_dir)
    cli::cli_alert_success("Backed up {backup_count} items to {backup_dir}/")
  }
  
  if (confirm) {
    cli::cli_alert_warning(
      "About to delete ALL {nrow(items)} item{?s} from gooseR memory!"
    )
    cli::cli_alert_danger("This action cannot be undone!")
    response <- readline("Type 'DELETE ALL' to confirm: ")
    if (response != "DELETE ALL") {
      cli::cli_alert_info("Cancelled - no items deleted")
      return(invisible(0))
    }
  }
  
  count <- 0
  cli::cli_progress_bar("Deleting all items", total = nrow(items))
  
  for (i in seq_len(nrow(items))) {
    tryCatch({
      goose_delete(
        name = items$name[i], 
        category = items$category[i],
        confirm = FALSE
      )
      count <- count + 1
      cli::cli_progress_update()
    }, error = function(e) {
      cli::cli_alert_danger(
        "Failed to delete '{items$name[i]}': {e$message}"
      )
    })
  }
  
  cli::cli_progress_done()
  cli::cli_alert_success("Cleared all gooseR memory ({count} items deleted)")
  
  invisible(count)
}

#' Check if an item exists in gooseR memory
#'
#' @param name Character string, name of the item
#' @param category Character string, category of the item (optional)
#'
#' @return Logical, TRUE if item exists
#' @export
#'
#' @examples
#' \dontrun{
#' if (goose_exists("my_data", "analysis")) {
#'   data <- goose_load("my_data", "analysis")
#' }
#' }
goose_exists <- function(name, category = NULL) {
  items <- tryCatch({
    if (is.null(category)) {
      goose_list()  # Search all categories
    } else {
      goose_list(category = category)
    }
  }, error = function(e) {
    return(data.frame())
  })
  
  # Handle NULL or empty result
  if (is.null(items) || nrow(items) == 0) {
    return(FALSE)
  }
  
  # Check if name exists, optionally filtering by category
  if (is.null(category)) {
    name %in% items$name
  } else {
    any(items$name == name & items$category == category)
  }
}

#' Rename an item in gooseR memory
#'
#' @param old_name Character string, current name
#' @param new_name Character string, new name
#' @param category Character string, category of the item
#'
#' @return Logical, TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' goose_rename("old_analysis", "final_analysis", category = "results")
#' }
goose_rename <- function(old_name, new_name, category = "general") {
  if (!goose_exists(old_name, category)) {
    cli::cli_alert_danger("Item '{old_name}' not found in category '{category}'")
    return(invisible(FALSE))
  }
  
  if (goose_exists(new_name, category)) {
    cli::cli_alert_danger("Item '{new_name}' already exists in category '{category}'")
    return(invisible(FALSE))
  }
  
  # Load the item
  item <- goose_load(old_name, category)
  
  # Get metadata if available
  items <- goose_list(category = category)
  old_item <- items[items$name == old_name, ]
  
  # Save with new name
  goose_save(
    item,
    name = new_name,
    category = category,
    tags = if ("tags" %in% names(old_item)) old_item$tags else NULL,
    description = if ("description" %in% names(old_item)) old_item$description else NULL
  )
  
  # Delete old item
  goose_delete(old_name, category, confirm = FALSE)
  
  cli::cli_alert_success("Renamed '{old_name}' to '{new_name}'")
  invisible(TRUE)
}

#' Create a backup of all gooseR memory
#'
#' @param backup_dir Character, directory to save backup
#' @param timestamp Logical, whether to add timestamp to backup files
#'
#' @return Invisible integer count of backed up items
#' @export
#'
#' @examples
#' \dontrun{
#' # Create backup with timestamp
#' goose_backup()
#' 
#' # Create backup in specific directory
#' goose_backup("my_backups")
#' }
goose_backup <- function(backup_dir = "goose_backup", timestamp = TRUE) {
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  items <- goose_list()
  if (nrow(items) == 0) {
    cli::cli_alert_info("No items to backup")
    return(invisible(0))
  }
  
  if (timestamp) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  } else {
    ts <- ""
  }
  
  count <- 0
  cli::cli_progress_bar("Creating backup", total = nrow(items))
  
  for (i in seq_len(nrow(items))) {
    tryCatch({
      obj <- goose_load(
        name = items$name[i],
        category = items$category[i]
      )
      
      filename <- if (timestamp) {
        file.path(backup_dir, sprintf("%s_%s_%s.rds", 
                                     ts, 
                                     items$category[i],
                                     items$name[i]))
      } else {
        file.path(backup_dir, sprintf("%s_%s.rds",
                                     items$category[i],
                                     items$name[i]))
      }
      
      saveRDS(obj, filename)
      count <- count + 1
      cli::cli_progress_update()
    }, error = function(e) {
      cli::cli_alert_danger(
        "Failed to backup '{items$name[i]}': {e$message}"
      )
    })
  }
  
  cli::cli_progress_done()
  cli::cli_alert_success("Backed up {count} items to {backup_dir}/")
  
  # Return the backup directory path
  invisible(file.path(getwd(), backup_dir))
}

# Session management environment
.goose_session <- new.env(parent = emptyenv())

#' Start a gooseR session for tracking saved items
#'
#' Note: To automatically tag items with the session ID, you need to
#' manually add the session tag when saving, or use the wrapper functions.
#'
#' @param session_id Character, optional session identifier
#'
#' @return Character, the session ID
#' @export
#'
#' @examples
#' \dontrun{
#' # Start a session
#' session_id <- goose_session_start()
#' 
#' # Save items (manually tag with session)
#' goose_save(mtcars, "cars_data", category = "analysis", 
#'            tags = c("myanalysis", getOption("goose.session_id")))
#' 
#' # See what was saved in this session
#' goose_session_list()
#' 
#' # Clean up session
#' goose_session_clear()
#' goose_session_end()
#' }
goose_session_start <- function(session_id = NULL) {
  if (is.null(session_id)) {
    session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  .goose_session$id <- session_id
  .goose_session$items <- character()
  .goose_session$start_time <- Sys.time()
  
  options(goose.session_id = session_id)
  
  cli::cli_alert_success("Started gooseR session: {session_id}")
  cli::cli_alert_info("Tag items with session ID to track them: tags = '{session_id}'")
  invisible(session_id)
}

#' List items saved in current session
#'
#' @return Data frame of session items
#' @export
goose_session_list <- function() {
  if (is.null(.goose_session$id)) {
    cli::cli_alert_info("No active session")
    return(data.frame())
  }
  
  session_tag <- .goose_session$id
  items <- tryCatch({
    goose_list(tags = session_tag)
  }, error = function(e) {
    data.frame()
  })
  
  # Handle NULL or empty result
  if (is.null(items) || nrow(items) == 0) {
    cli::cli_alert_info("No items saved in current session")
    return(data.frame())
  }
  
  cli::cli_alert_info("Session '{(.goose_session$id)}' has {nrow(items)} item{?s}")
  items
}

#' Clear all items from current session
#'
#' @param confirm Logical, whether to confirm deletion
#'
#' @return Invisible integer count of deleted items
#' @export
goose_session_clear <- function(confirm = TRUE) {
  if (is.null(.goose_session$id)) {
    cli::cli_alert_info("No active session")
    return(invisible(0))
  }
  
  count <- goose_clear_tags(.goose_session$id, confirm = confirm, verbose = TRUE)
  invisible(count)
}

#' End the current gooseR session
#'
#' @param cleanup Logical, whether to delete session items
#'
#' @return Invisible NULL
#' @export
goose_session_end <- function(cleanup = FALSE) {
  if (is.null(.goose_session$id)) {
    cli::cli_alert_info("No active session to end")
    return(invisible(NULL))
  }
  
  session_id <- .goose_session$id
  duration <- difftime(Sys.time(), .goose_session$start_time, units = "mins")
  
  if (cleanup) {
    count <- goose_session_clear(confirm = FALSE)
    cli::cli_alert_info("Cleaned up {count} item{?s} from session")
  }
  
  # Clear session
  rm(list = ls(.goose_session), envir = .goose_session)
  options(goose.session_id = NULL)
  
  cli::cli_alert_success(
    "Ended session '{session_id}' (duration: {round(duration, 1)} minutes)"
  )
  
  invisible(NULL)
}

#' Execute code with automatic gooseR session management
#'
#' @param expr Expression to evaluate
#' @param cleanup Logical, whether to clean up after execution
#' @param session_id Optional session identifier
#'
#' @return Result of expression
#' @export
#'
#' @examples
#' \dontrun{
#' # Run analysis with automatic cleanup
#' result <- with_goose_session({
#'   goose_save(mtcars, "cars", category = "temp")
#'   goose_save(iris, "flowers", category = "temp")
#'   # Do analysis...
#'   "Analysis complete"
#' }, cleanup = TRUE)
#' }
with_goose_session <- function(expr, cleanup = TRUE, session_id = NULL) {
  # Start session
  sid <- goose_session_start(session_id)
  
  # Ensure cleanup on exit
  on.exit(goose_session_end(cleanup = cleanup), add = TRUE)
  
  # Create a modified environment where goose_save adds session tag
  env <- new.env(parent = parent.frame())
  
  # Override goose_save in this environment
  env$goose_save <- function(..., tags = NULL) {
    # Add session tag to any existing tags
    session_tag <- getOption("goose.session_id")
    if (!is.null(session_tag)) {
      tags <- unique(c(tags, session_tag))
    }
    # Call the original goose_save
    gooseR::goose_save(..., tags = tags)
  }
  
  # Evaluate expression in modified environment
  eval(expr, envir = env)
}

#' Save an object with session tracking
#'
#' A wrapper around goose_save that automatically adds the current session tag
#'
#' @param ... Arguments passed to goose_save
#' @param tags Additional tags (session tag will be added automatically)
#'
#' @return Same as goose_save
#' @export
goose_session_save <- function(..., tags = NULL) {
  session_tag <- getOption("goose.session_id")
  if (!is.null(session_tag)) {
    tags <- unique(c(tags, session_tag))
  }
  goose_save(..., tags = tags)
}
