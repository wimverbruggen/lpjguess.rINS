#' Resolve imports. Internally used function.
#' 
#' @param file_path Path to an ins file
#' @param base_dir Directory in which the ins files can be found
#' @return INS file structure where all imports are resolved
#' @export
resolve_imports <- function(file_path, base_dir = NULL, processed_files = character()) {
  if (is.null(base_dir)) {
    base_dir <- dirname(file_path)
  }
  
  # Check for circular imports
  if (file_path %in% processed_files) {
    warning("Circular import detected for file: ", file_path)
    return(character())
  }
  
  processed_files <- c(processed_files, file_path)
  
  # Read the file content
  content <- readLines(file_path)
  
  # Process each line for imports
  resolved_content <- character()
  
  for (line in content) {
    # Check for import statements
    if (str_detect(line, '^import\\s+"([^"]+)"')) {
      import_file <- str_match(line, '^import\\s+"([^"]+)"')[1,2]
      import_path <- file.path(base_dir, import_file)
      
      if (file.exists(import_path)) {
        cat("Importing:", import_path, "\n")
        # Recursively resolve imports in the imported file
        imported_content <- resolve_imports(import_path, dirname(import_path), processed_files)
        resolved_content <- c(resolved_content, imported_content)
      } else {
        warning("Import file not found: ", import_path)
      }
    } else {
      # Keep the original line
      resolved_content <- c(resolved_content, line)
    }
  }
  
  return(resolved_content)
}