#!/usr/bin/env Rscript

renv::load("/Users/mgierlinski/Projects/DDUpred")

suppressPackageStartupMessages({
  library(optparse)
  library(drupr)
  library(tidyverse)
})

option_list <- list(
  make_option(c("-t", "--training-file"), action = "store", default = NA, type = 'character',
              help = "Training data file. A CSV file with training data."),
  make_option(c("-i", "--info-file"), action = "store", default = NA, type = 'character',
              help = "Training data variable info file. A CSV file with two columns: 'variable' and 'type'. The first column contains variable names, the second 'id' for identifier column, 'response' for response variables or 'none' for variables not to be used."),
  make_option(c("-s", "--test-file"), action = "store", default = NA, type = 'character',
              help = "Test data file"),
  make_option(c("-o", "--output-dir"), action = "store", default = NA, type = 'character',
              help = "Output directory where all results will be written."),
  make_option(c("-m", "--min-good"), action = "store", default = NA, type = 'integer',
              help = "Minimum number of good values in each training variable. Only variables (columns) in the training file with at least that many non-missing values will be used. Results might vary, but using about two-thirds of the number of the training compunds will do.")
)

cat(paste("\n  RFpred", packageVersion("drupr"), "\n\n"))

op <- OptionParser(option_list = option_list)
opt <- parse_args(op)

if (is.na(opt$`training-file`) | is.na(opt$`info-file`) | is.na(opt$`test-file`) | is.na(opt$`output-dir`) | is.na(opt$`min-good`)) {
  print_help(op)
  stop("Option(s) missing")
}

write_res <- function(d, odir, file) {
  d %>%
    mutate_if(is.numeric, ~signif(.x, 4)) %>%
    write_csv(file.path(odir, file))
}

write_results <- function(pred, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir)
  
  write_res(pred$missing, out_dir, "missing.csv")
  write_res(pred$predictions, out_dir, "predictions.csv")
  write_res(pred$importance, out_dir, "importance.csv")
  write_res(pred$train_variables, out_dir, "train_variables.csv")
  write_res(pred$variable_comparison, out_dir, "variable_comparison.csv")
  write_res(pred$mismatched_levels, out_dir, "mismatched_levels.csv")
}


# Predictions

pred <- ddu_prediction(opt$`training-file`, opt$`info-file`, opt$`test-file`, verbose = TRUE, min_unique = 2, min_good = opt$`min-good`, max_cat_levels = 10)

# Write results

write_results(pred, opt$`output-dir`)


