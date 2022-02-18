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


read_excel_ph <- function(file, verbose=FALSE) {
  if(verbose) cat(paste("\nReading data from", file, "\n"))
  d <- readxl::read_excel(file) %>% 
    rename(Name = ID)
  if(verbose) cat(paste("   ", nrow(d), "rows,", ncol(d), "columns\n"))
  d
}

merge_ph <- function(desc_file, ph2_file, verbose=TRUE) {
  desc <- read_excel_ph(desc_file, verbose)
  ph2 <- read_excel_ph(ph2_file, verbose)

  ph2_sel <- ph2 %>% 
    select("Name", where(is.numeric))
  ph_cols <- colnames(ph2_sel)[2:ncol(ph2_sel)]
  if(verbose) cat(paste("\npH columns found:", paste(ph_cols, collapse = ", "), "\n"))
  
  desc_cols <- desc %>% 
    select(where(is.numeric)) %>% 
    colnames()
  
  mtch <- ph_cols %in% desc_cols
  if(!all(mtch)) {
    cat(paste("\nError: pH columns not found in the descriptor file:", paste(ph_cols[!mtch], collapse = ","), "\n"))
    stop()
  }
  
  desc_sel <- desc %>% 
    select("Name", all_of(ph_cols))
  desc_rest <- desc %>% 
    select(-all_of(ph_cols))
  
  ph2_names <- glue::glue("G+_pH2_{ph_cols}")
  desc_names <- glue::glue("G+_ph7.4_{ph_cols}")
  
  if(verbose) cat(paste("\nCreating columns", paste(c(ph2_names, desc_names), collapse = ", "), "in the output file.\n"))
  
  colnames(ph2_sel) <- c("Name", ph2_names)
  colnames(desc_sel) <- c("Name", desc_names)
  
  desc_rest %>% 
    left_join(desc_sel, by = "Name") %>% 
    left_join(ph2_sel, by = "Name")
}