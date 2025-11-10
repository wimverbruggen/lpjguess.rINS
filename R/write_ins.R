#' Write ins from structured list.
#' 
#' @param params Structured list with all parameter info
#' @param output_file Path to .ins file that will be created
#' @export
write_ins <- function(params, output_file) {
  lines <- character()
  
  # Helper function to format parameter values
  format_value <- function(value) {
    if (is.numeric(value)) {
      return(as.character(value))
    } else if (is.character(value)) {
      return(paste0('"', value, '"'))
    } else if (length(value) > 1) {
      # Vector of values - write space-separated on one line
      return(paste(sapply(value, function(x) {
        if (is.character(x)) paste0('"', x, '"') else as.character(x)
      }), collapse = " "))
    } else {
      return(as.character(value))
    }
  }
  
  # Write model parameters
  if (length(params$model) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! MODEL PARAMETERS")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    # Write param statements first (each on separate lines)
    if (!is.null(params$model$param)) {
      for (param_line in params$model$param) {
        lines <- c(lines, paste("param", param_line))
      }
      lines <- c(lines, "")
    }
    
    # Write other model parameters
    for (param_name in names(params$model)) {
      if (param_name != "param") {
        value <- params$model[[param_name]]
        lines <- c(lines, paste(param_name, format_value(value)))
      }
    }
    lines <- c(lines, "")
  }
  
  # Write groups
  if (length(params$group) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! PARAMETER GROUPS")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (group_name in names(params$group)) {
      lines <- c(lines, paste0('group "', group_name, '" ('))
      lines <- c(lines, "")
      
      group_data <- params$group[[group_name]]
      
      # Write imports first
      if (!is.null(group_data$imports)) {
        for (import_name in group_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters - each on single line even if multi-value
      other_params <- group_data[!names(group_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        #lines <- c(lines, paste("  ", param_name, format_value(value)))
        lines <- c(lines, paste("  ",param_name,str_flatten(format_value(value),collapse = " ")))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write stand types (st)
  if (length(params$st) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! STAND TYPES")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (st_name in names(params$st)) {
      lines <- c(lines, paste0('st "', st_name, '" ('))
      lines <- c(lines, "")
      
      st_data <- params$st[[st_name]]
      
      # Write imports first
      if (!is.null(st_data$imports)) {
        for (import_name in st_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters - each on single line even if multi-value
      other_params <- st_data[!names(st_data) %in% "imports"]
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        lines <- c(lines, paste("  ", param_name, format_value(value)))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write PFTs
  if (length(params$pft) > 0) {
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "! PLANT FUNCTIONAL TYPES")
    lines <- c(lines, "!///////////////////////////////////////////////////////////////////////////////")
    lines <- c(lines, "")
    
    for (pft_name in names(params$pft)) {
      lines <- c(lines, paste0('pft "', pft_name, '" ('))
      lines <- c(lines, "")
      
      pft_data <- params$pft[[pft_name]]
      
      # Write imports first
      if (!is.null(pft_data$imports)) {
        for (import_name in pft_data$imports) {
          lines <- c(lines, paste("  ", import_name))
        }
        lines <- c(lines, "")
      }
      
      # Write other parameters (include should come early)
      other_params <- pft_data[!names(pft_data) %in% "imports"]
      
      # Write include parameter first if it exists
      if (!is.null(other_params$include)) {
        lines <- c(lines, paste("  include", format_value(other_params$include)))
        other_params$include <- NULL
      }
      
      # Write remaining parameters - each on single line even if multi-value
      for (param_name in names(other_params)) {
        value <- other_params[[param_name]]
        #lines <- c(lines, paste("  ", param_name, format_value(value)))
        lines <- c(lines, paste("  ",param_name,str_flatten(format_value(value),collapse = " ")))
      }
      
      lines <- c(lines, ")")
      lines <- c(lines, "")
    }
  }
  
  # Write to file
  writeLines(lines, output_file)
  cat("INS file written to:", output_file, "\n")
}