# convert_sas_to_parquet

# add a file.size limit
# maybe add a file.size warning and loop to big files
# also add sanitizer

convert_sas_to_parquet <- function(old_path, new_path, file_no_ext){
  
  old_path_file <- paste0(old_path, "/", file_no_ext, ".sas7bdat")
  new_path_file <- paste0(new_path, "/", file_no_ext, ".parquet")
  
  arrow::write_parquet(haven::read_sas(old_path_file), new_path_file)
  
  cat("Converted:", file_no_ext, "to parquet", "\n")
}