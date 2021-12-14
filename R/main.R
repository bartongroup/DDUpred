VERSION <- "v0.1.0"

suppressPackageStartupMessages({
  library(fitdistrplus)
  library(randomForest)
  library(tidyverse)
})
source("R/process.R")
source("R/variables.R")
source("R/io.R")
source("R/grouping.R")
source("R/random_forest.R")

select <- dplyr::select

ddu_prediction <- function(train_file, info_file, test_file, verbose=TRUE, min_unique=2, min_good=1500, max_cat_levels=10) {
  
  train_raw <- import_data(train_file, verbose)
  variable_info <- read_csv(info_file, show_col_types = FALSE, progress = FALSE)
  train_set <- process_training_data(train_raw, variable_info, verbose = verbose)
  
  test_raw <- import_data(test_file, verbose)
  test_set <- process_test_data(test_raw, train_set, verbose = verbose)

  predict_new_rf_exps(train_set, test_set, min_unique, min_good, max_cat_levels, verbose=verbose)
}


