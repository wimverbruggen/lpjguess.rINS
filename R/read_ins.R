#' @title Read LPJ-GUESS ins file
#' @description Read *.ins files. Automatically include imported ins files, and return a list structure with all values.
#' 
#' @param file_path Path to LPJ-GUESS ins file (can import other ins files!)
#' @import dplyr
#' @import stringr
#' @return List structure containing contents of ins file
#' @export
read_ins <- function(file_path) {
  
  cat("Processing file:", file_path, "\n")
  
  # Step 1: Resolve all imports recursively
  full_content <- resolve_imports(file_path)
  
  # Step 2: Remove comments and clean lines
  content_clean <- full_content %>% 
    str_replace("!.*", "") %>%  # Remove comments
    str_trim() %>%             # Remove leading/trailing whitespace
    .[. != ""]                 # Remove empty lines
  
  # Step 3: Parse the cleaned content
  result <- list(
    model = list(),
    st = list(),
    group = list(),
    pft = list()
  )
  
  current_type <- NA
  current_name <- NA
  current_params <- list()
  bracket_count <- 0
  in_section <- FALSE
  
  # Process each line
  for (i in seq_along(content_clean)) {
    line <- content_clean[i]
    
    # Check for st (stand type) declaration
    if (str_detect(line, "^st\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new st
      current_name <- str_match(line, "^st\\s+\"([^\"]+)\"")[1,2]
      current_type <- "st"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (str_detect(line, "^group\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new group
      current_name <- str_match(line, "^group\\s+\"([^\"]+)\"")[1,2]
      current_type <- "group"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (str_detect(line, "^pft\\s+\"([^\"]+)\"\\s*\\(")) {
      # Save previous section if exists
      if (in_section && !is.na(current_name)) {
        result[[current_type]][[current_name]] <- current_params
      }
      
      # Start new pft
      current_name <- str_match(line, "^pft\\s+\"([^\"]+)\"")[1,2]
      current_type <- "pft"
      current_params <- list(imports = character())
      bracket_count <- 1
      in_section <- TRUE
      
    } else if (!in_section) {
      # Only parse model parameters if we're NOT in a section
      # Handle param statements (special case)
      if (str_detect(line, '^param\\s+')) {
        # Store the entire line after "param" as the value with key "param"
        param_value <- str_trim(str_sub(line, 6))  # Remove "param" prefix
        if (is.null(result$model$param)) {
          result$model$param <- character()
        }
        result$model$param <- c(result$model$param, param_value)
      } else if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s+")) {
        # Regular model parameter (key-value pair) - but NOT if it's a section start
        tokens <- str_split(line, "\\s+", n = 2)[[1]]
        if (length(tokens) >= 2) {
          param_name <- tokens[1]
          param_value <- str_trim(tokens[2])
          
          # Skip if this looks like a section start that wasn't caught
          if (param_name %in% c("st", "group", "pft")) {
            next
          }
          
          # Remove quotes if present
          param_value <- str_remove_all(param_value, "^\"|\"$")
          
          # Convert to numeric if possible
          param_value_numeric <- suppressWarnings(as.numeric(param_value))
          if (!is.na(param_value_numeric)) {
            param_value <- param_value_numeric
          }
          
          result$model[[param_name]] <- param_value
        }
      } else if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_]*\\s*\".*\"$")) {
        # Model parameter with quoted string value
        tokens <- str_split(line, "\\s+", n = 2)[[1]]
        if (length(tokens) >= 2) {
          param_name <- tokens[1]
          param_value <- str_trim(tokens[2])
          
          # Skip if this looks like a section start
          if (param_name %in% c("st", "group", "pft")) {
            next
          }
          
          param_value <- str_remove_all(param_value, "^\"|\"$")
          result$model[[param_name]] <- param_value
        }
      }
    } else if (in_section) {
      # Handle brackets
      if (str_detect(line, "\\(")) {
        bracket_count <- bracket_count + str_count(line, "\\(")
      }
      if (str_detect(line, "\\)")) {
        bracket_count <- bracket_count - str_count(line, "\\)")
      }
      
      # Check if section ended
      if (bracket_count == 0) {
        result[[current_type]][[current_name]] <- current_params
        in_section <- FALSE
        current_name <- NA
        current_type <- NA
        next
      }
      
      # Parse content inside section
      tokens <- str_split(line, "\\s+")[[1]]
      tokens <- tokens[tokens != ""]
      
      if (length(tokens) == 0) next
      
      # Check for standalone names (inheritance for groups and pfts)
      if ((current_type == "group" || current_type == "pft" || current_type == "st") &&
          length(tokens) == 1 && 
          !str_detect(tokens[1], "^-?[0-9]")) {
        # This is a group/stand name for inheritance
        current_params$imports <- c(current_params$imports, tokens[1])
      } else if (length(tokens) >= 2) {
        # Regular parameter
        param_name <- tokens[1]
        param_values <- tokens[-1]
        
        # Remove quotes from individual values if they are strings
        param_values <- sapply(param_values, function(x) {
          if (str_detect(x, '^".*"$')) {
            str_remove_all(x, '^"|"$')
          } else {
            x
          }
        })
        
        # Convert to numeric if possible
        param_values_numeric <- suppressWarnings(as.numeric(param_values))
        if (!any(is.na(param_values_numeric))) {
          param_values <- param_values_numeric
        }
        
        # Store single values as scalars, multiple values as vectors
        if (length(param_values) == 1) {
          current_params[[param_name]] <- param_values[[1]]
        } else {
          current_params[[param_name]] <- param_values
        }
      }
    }
  }
  
  # Save the last section if exists
  if (in_section && !is.na(current_name)) {
    result[[current_type]][[current_name]] <- current_params
  }
  
  return(result)
}
