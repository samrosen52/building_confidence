# dir_functions

# Organizational functions to create folder from script


# Get base name of script with extension
get_script_name <- function() {
  script_path <- rstudioapi::getActiveDocumentContext()
  if(!is.null(script_path) && nzchar(script_path$path)) {
    return(basename(script_path$path))
  }
  return(NA)
}

# Get base name of script without extension
get_script_name_no_ext <- function() {
  tools::file_path_sans_ext(get_script_name())
}

# Get base path of script
get_script_path <- function() {
  script_path <- rstudioapi::getActiveDocumentContext()
  if(!is.null(script_path$path) && nzchar(script_path$path)) {
    return(script_path$path)
  }
  return(NA)
}

# Create a parallel directory
create_parallel_dir <- function(target_dir) {
  
  # Get current script path in RStudio
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if(is.null(script_path) || !nzchar(script_path)) {
    stop("Cannot determine current script path.")
  }
  
  # Directory containing current script
  script_dir <- dirname(script_path)
  
  # Go to project root directory
  project_root <- dirname(script_dir)
  
  # Get a script name without extension
  script_name <- tools::file_path_sans_ext(basename(script_path))
  
  # Build path to parallel directory
  out_dir <- file_path(project_root, target_dir, script_name)
  
  # Create directory if it does not exist
  if(!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  return(normalizePath(out_dir))
}


# NOTE: combine these functions together

# Get parallel directory path
get_parallel_dir <- function(target_dir) {
  
  # Get current script path in RStudio
  script_path <- rstudioapi::getActiveDocumentContext()$path
  
  if(is.null(script_path) || !nzchar(script_path)) {
    stop("Cannot determine current script path.")
  }
  # Directory containing current script
  script_dir <- dirname(script_path)
  
  # Go to project root directory
  project_root <- dirname(script_dir)
  
  # Get a script name without extension
  script_name <- tools::file_path_sans_ext(basename(script_path))
  
  # Build path to parallel directory
  out_dir <- file_path(project_root, target_dir, script_name)
  
  return(normalizePath(out_dir))
  
}












