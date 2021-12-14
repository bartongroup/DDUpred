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
  write_res(pred$mismatched_levels, out_dir, "mismatched_levels.csv")
}



# The test data Inem_18_mastersheet.csv is incompatible with the older Lombardo
# training data in the file gastroplus.csv. Some columns need renaming. This
# script does the job, it should be ran once.
rename_for_vd <- function() {
  r <- read_csv("data/lombardo_rename.csv", show_col_types = FALSE)
  ren <- set_names(r$renamed_variable, r$original_variable)
  
  read_csv("data/Inem_18_mastersheet.csv", show_col_types = FALSE) %>% 
    rename(ren) %>% 
    write_csv("data/Inem_18_mastersheet_lombardo.csv")
}