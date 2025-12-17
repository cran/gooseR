#' Get Goose Memory Directory Path
#'
#' @param global Logical. If TRUE, returns global memory path. If FALSE, returns local project memory path.
#' @param use_temp Logical. If TRUE (default for CRAN compliance), uses tempdir(). Set to FALSE for production use.
#' @return Character string with the memory directory path
#' @keywords internal
get_memory_path <- function(global = TRUE, use_temp = TRUE) {
  if (use_temp) {
    # Use temporary directory (CRAN policy compliant)
    return(fs::path(tempdir(), "gooseR_memory"))
  }
  
  if (global) {
    fs::path(fs::path_home(), ".config", "goose", "memory")
  } else {
    # Local project memory
    project_root <- here::here()
    fs::path(project_root, ".goose", "memory")
  }
}

#' Save R Object to Goose Memory
#'
#' Save an R object to Goose's memory system with optional tags and category.
#' Objects are serialized to RDS format and metadata is stored in Goose's text format.
#'
#' @param object The R object to save (can be any R object: data.frame, model, list, etc.)
#' @param name Character string. Name for the saved object (will be used as filename)
#' @param category Character string. Category for organizing memories (default: "r_objects")
#' @param tags Character vector. Optional tags for searching/filtering
#' @param description Character string. Optional description of the object
#' @param global Logical. If TRUE (default), saves to global Goose memory. If FALSE, saves to project-local memory.
#' @param overwrite Logical. If TRUE, overwrites existing object. If FALSE (default), errors if object exists.
#'
#' @return Invisibly returns the path where the object was saved
#' @export
#'
#' @examples
#' \dontrun{
#' # Save a model
#' model <- lm(mpg ~ wt, data = mtcars)
#' goose_save(model, "mtcars_model", 
#'            category = "models",
#'            tags = c("regression", "mtcars"),
#'            description = "Linear model predicting mpg from weight")
#'
#' # Save a data frame
#' goose_save(iris, "iris_data", 
#'            category = "datasets",
#'            tags = "example")
#' }
goose_save <- function(object, 
                       name, 
                       category = "r_objects",
                       tags = NULL,
                       description = NULL,
                       global = TRUE,
                       overwrite = FALSE) {
  
  # Validate inputs
  if (missing(object)) {
    cli::cli_abort("Must provide an {.arg object} to save")
  }
  if (missing(name) || !is.character(name) || length(name) != 1) {
    cli::cli_abort("{.arg name} must be a single character string")
  }
  
  # Clean name (remove special characters, spaces)
  name <- gsub("[^[:alnum:]_-]", "_", name)
  
  # Get memory path
  memory_dir <- get_memory_path(global)
  
  # Create memory directory if it doesn't exist
  if (!fs::dir_exists(memory_dir)) {
    fs::dir_create(memory_dir, recurse = TRUE)
    cli::cli_alert_info("Created Goose memory directory: {.path {memory_dir}}")
  }
  
  # Create RDS storage directory
  rds_dir <- fs::path(memory_dir, "r_objects")
  if (!fs::dir_exists(rds_dir)) {
    fs::dir_create(rds_dir)
  }
  
  # File paths
  rds_path <- fs::path(rds_dir, paste0(name, ".rds"))
  metadata_path <- fs::path(memory_dir, paste0(category, ".txt"))
  
  # Check if object already exists
  if (fs::file_exists(rds_path) && !overwrite) {
    cli::cli_abort(c(
      "Object {.val {name}} already exists in category {.val {category}}",
      "i" = "Use {.code overwrite = TRUE} to replace it"
    ))
  }
  
  # Save the R object as RDS
  saveRDS(object, rds_path)
  
  # Create metadata entry for Goose memory
  metadata <- create_memory_entry(
    name = name,
    category = category,
    tags = tags,
    description = description,
    rds_path = as.character(rds_path),
    object_class = class(object)[1],
    object_size = format(object.size(object), units = "auto")
  )
  
  # Append or update metadata in category file
  update_memory_metadata(metadata_path, metadata, name, overwrite)
  
  # Success message
  cli::cli_alert_success(
    "Saved {.val {name}} to Goose memory (category: {.val {category}})"
  )
  
  if (!is.null(tags)) {
    cli::cli_alert_info("Tags: {.val {tags}}")
  }
  
  invisible(rds_path)
}

#' Load R Object from Goose Memory
#'
#' Retrieve a previously saved R object from Goose's memory system.
#'
#' @param name Character string. Name of the object to load
#' @param category Character string. Category where the object was saved (default: "r_objects")
#' @param global Logical. If TRUE (default), loads from global memory. If FALSE, loads from project-local memory.
#'
#' @return The R object that was saved
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a previously saved model
#' model <- goose_load("mtcars_model", category = "models")
#' 
#' # Load a dataset
#' data <- goose_load("iris_data", category = "datasets")
#' }
goose_load <- function(name, category = "r_objects", global = TRUE) {
  
  # Validate inputs
  if (missing(name) || !is.character(name) || length(name) != 1) {
    cli::cli_abort("{.arg name} must be a single character string")
  }
  
  # Clean name
  name <- gsub("[^[:alnum:]_-]", "_", name)
  
  # Get memory path
  memory_dir <- get_memory_path(global)
  rds_dir <- fs::path(memory_dir, "r_objects")
  rds_path <- fs::path(rds_dir, paste0(name, ".rds"))
  
  # Check if file exists
  if (!fs::file_exists(rds_path)) {
    cli::cli_abort(c(
      "Object {.val {name}} not found in category {.val {category}}",
      "i" = "Use {.code goose_list()} to see available objects"
    ))
  }
  
  # Load the object
  obj <- readRDS(rds_path)
  
  cli::cli_alert_success("Loaded {.val {name}} from Goose memory")
  
  return(obj)
}

#' List Objects in Goose Memory
#'
#' List all R objects saved in Goose memory, optionally filtered by category or tags.
#'
#' @param category Character string. Filter by category (default: NULL shows all)
#' @param tags Character vector. Filter by tags (default: NULL shows all)
#' @param global Logical. If TRUE (default), lists global memory. If FALSE, lists project-local memory.
#'
#' @return A data.frame with information about saved objects
#' @export
#'
#' @examples
#' \dontrun{
#' # List all saved objects
#' goose_list()
#' 
#' # List objects in a specific category
#' goose_list(category = "models")
#' 
#' # List objects with specific tags
#' goose_list(tags = "production")
#' }
goose_list <- function(category = NULL, tags = NULL, global = TRUE) {
  
  memory_dir <- get_memory_path(global)
  
  if (!fs::dir_exists(memory_dir)) {
    cli::cli_alert_warning("No Goose memory found. Use {.code goose_save()} to create memories.")
    return(invisible(NULL))
  }
  
  # Get all category files
  category_files <- fs::dir_ls(memory_dir, glob = "*.txt")
  
  if (length(category_files) == 0) {
    cli::cli_alert_warning("No memories found in Goose memory.")
    return(invisible(NULL))
  }
  
  # Parse all memory entries
  all_memories <- purrr::map_dfr(category_files, parse_memory_file)
  
  # Filter by category if specified
  if (!is.null(category)) {
    all_memories <- all_memories[all_memories$category == category, ]
  }
  
  # Filter by tags if specified
  if (!is.null(tags)) {
    all_memories <- all_memories[
      purrr::map_lgl(all_memories$tags, ~any(tags %in% .x)),
    ]
  }
  
  if (nrow(all_memories) == 0) {
    cli::cli_alert_warning("No memories found matching your criteria.")
    return(invisible(NULL))
  }
  
  # Print summary
  cli::cli_h2("Goose Memory Objects")
  cli::cli_text("Found {.val {nrow(all_memories)}} object{?s}")
  
  print(all_memories)
  
  invisible(all_memories)
}

#' Delete Object from Goose Memory
#'
#' Remove a saved R object from Goose's memory system.
#'
#' @param name Character string. Name of the object to delete
#' @param category Character string. Category where the object was saved (default: "r_objects")
#' @param global Logical. If TRUE (default), deletes from global memory. If FALSE, deletes from project-local memory.
#' @param confirm Logical. If TRUE (default), asks for confirmation before deleting.
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Delete an object
#' goose_delete("old_model", category = "models")
#' 
#' # Delete without confirmation
#' goose_delete("temp_data", confirm = FALSE)
#' }
goose_delete <- function(name, category = "r_objects", global = TRUE, confirm = TRUE) {
  
  # Validate inputs
  if (missing(name) || !is.character(name) || length(name) != 1) {
    cli::cli_abort("{.arg name} must be a single character string")
  }
  
  # Clean name
  name <- gsub("[^[:alnum:]_-]", "_", name)
  
  # Get paths
  memory_dir <- get_memory_path(global)
  rds_dir <- fs::path(memory_dir, "r_objects")
  rds_path <- fs::path(rds_dir, paste0(name, ".rds"))
  
  # Check if file exists
  if (!fs::file_exists(rds_path)) {
    cli::cli_abort("Object {.val {name}} not found in category {.val {category}}")
  }
  
  # Confirm deletion
  if (confirm) {
    response <- readline(sprintf("Delete '%s' from Goose memory? (y/n): ", name))
    if (!tolower(response) %in% c("y", "yes")) {
      cli::cli_alert_info("Deletion cancelled")
      return(invisible(FALSE))
    }
  }
  
  # Delete RDS file
  fs::file_delete(rds_path)
  
  # Remove from metadata
  metadata_path <- fs::path(memory_dir, paste0(category, ".txt"))
  if (fs::file_exists(metadata_path)) {
    remove_memory_entry(metadata_path, name)
  }
  
  cli::cli_alert_success("Deleted {.val {name}} from Goose memory")
  
  invisible(TRUE)
}

#' Helper: Create Memory Entry
#' @keywords internal
create_memory_entry <- function(name, category, tags, description, 
                                rds_path, object_class, object_size) {
  
  # Format tags
  tags_str <- if (!is.null(tags)) {
    paste(tags, collapse = " ")
  } else {
    ""
  }
  
  # Create entry in Goose memory format
  entry <- paste0(
    "# ", tags_str, "\n",
    "R_OBJECT: ", name, "\n",
    "CLASS: ", object_class, "\n",
    "SIZE: ", object_size, "\n",
    "PATH: ", rds_path, "\n"
  )
  
  if (!is.null(description)) {
    entry <- paste0(entry, "DESCRIPTION: ", description, "\n")
  }
  
  entry <- paste0(entry, "SAVED: ", Sys.time(), "\n")
  
  return(entry)
}

#' Helper: Update Memory Metadata File
#' @keywords internal
update_memory_metadata <- function(metadata_path, metadata, name, overwrite) {
  
  if (fs::file_exists(metadata_path)) {
    # Read existing content
    existing <- readLines(metadata_path, warn = FALSE)
    
    if (overwrite) {
      # Remove old entry if it exists
      existing <- remove_entry_from_lines(existing, name)
    }
    
    # Append new entry
    writeLines(c(existing, "", metadata), metadata_path)
  } else {
    # Create new file
    writeLines(metadata, metadata_path)
  }
}

#' Helper: Remove Memory Entry from Metadata
#' @keywords internal
remove_memory_entry <- function(metadata_path, name) {
  if (!fs::file_exists(metadata_path)) {
    return(invisible(NULL))
  }
  
  lines <- readLines(metadata_path, warn = FALSE)
  lines <- remove_entry_from_lines(lines, name)
  writeLines(lines, metadata_path)
}

#' Helper: Remove Entry from Lines
#' @keywords internal
remove_entry_from_lines <- function(lines, name) {
  # Find the entry for this object
  name_pattern <- paste0("^R_OBJECT: ", name, "$")
  name_indices <- grep(name_pattern, lines)
  
  if (length(name_indices) == 0) {
    return(lines)
  }
  
  # Find the start of the entry (previous # line)
  start_idx <- name_indices[1]
  while (start_idx > 1 && !grepl("^#", lines[start_idx])) {
    start_idx <- start_idx - 1
  }
  
  # Find the end of the entry (next # line or end of file)
  end_idx <- name_indices[1]
  while (end_idx < length(lines) && !grepl("^#", lines[end_idx + 1])) {
    end_idx <- end_idx + 1
  }
  
  # Remove the entry
  if (start_idx <= end_idx) {
    lines <- lines[-(start_idx:end_idx)]
  }
  
  return(lines)
}

#' Helper: Parse Memory File
#' @keywords internal
parse_memory_file <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  category <- tools::file_path_sans_ext(basename(file_path))
  
  # Find all R_OBJECT entries
  object_lines <- grep("^R_OBJECT:", lines)
  
  if (length(object_lines) == 0) {
    return(data.frame())
  }
  
  # Parse each entry
  purrr::map_dfr(object_lines, function(idx) {
    # Get the entry block
    start_idx <- idx
    while (start_idx > 1 && !grepl("^#", lines[start_idx - 1])) {
      start_idx <- start_idx - 1
    }
    if (start_idx > 1 && grepl("^#", lines[start_idx - 1])) {
      start_idx <- start_idx - 1
    }
    
    end_idx <- idx
    while (end_idx < length(lines) && !grepl("^R_OBJECT:", lines[end_idx + 1])) {
      end_idx <- end_idx + 1
      if (end_idx >= length(lines)) break
    }
    
    entry_lines <- lines[start_idx:end_idx]
    
    # Parse fields
    name <- sub("^R_OBJECT: ", "", entry_lines[grep("^R_OBJECT:", entry_lines)])
    class <- sub("^CLASS: ", "", entry_lines[grep("^CLASS:", entry_lines)])
    size <- sub("^SIZE: ", "", entry_lines[grep("^SIZE:", entry_lines)])
    
    tags_line <- entry_lines[grep("^#", entry_lines)]
    tags <- if (length(tags_line) > 0) {
      strsplit(sub("^# ", "", tags_line), " ")[[1]]
    } else {
      character(0)
    }
    
    data.frame(
      name = name,
      category = category,
      class = class,
      size = size,
      tags = I(list(tags)),
      stringsAsFactors = FALSE
    )
  })
}
