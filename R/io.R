import_data <- function(file, verbose=FALSE) {
  if(verbose) cat(paste("\nReading data from", file, "\n"))
  d <- read_csv(file, col_types = cols(), na=c("", "NA", "n/a", "N/A", "None"), guess_max = 10000, progress = FALSE)
  if(verbose) cat(paste("   ", nrow(d), "rows,", ncol(d), "columns\n"))
  return(d)
}



write_res <- function(d, odir, file) {
  d %>% 
    mutate_if(is.numeric, ~signif(.x, 4)) %>% 
    write_csv(file.path(odir, file))
}

write_results <- function(pred, out_dir) {
  if(!dir.exists(out_dir)) dir.create(out_dir)
  
  write_res(pred$predictions, out_dir, "predictions.csv")
  write_res(pred$importance, out_dir, "importance.csv")
  write_res(pred$train_variables, out_dir, "train_variables.csv")
  write_res(pred$variable_comparison, out_dir, "variable_comparison.csv")
}