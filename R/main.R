library(fitdistrplus)
library(randomForest)
library(tidyverse)
source("R/processing.R")
source("R/io.R")
source("R/grouping.R")
source("R/random_forest.R")

select <- dplyr::select

EXP_VARIABLES <- c("Exp_CHI-LOGD", "Exp_GSK-ChromLogD", "Exp_PAMPA-Papp", "Exp_realSOL_7.4", "log Exp_CLi Heps mouse", "log Exp_CLi Mics mouse", "log Exp_PPB mouse")

ddu_prediction <- function(train_file, info_file, test_file) {
  train_raw <- import_data(train_file)
  package_info <- read_csv(info_file, show_col_types = FALSE)
  
  train_set <- process_raw(train_raw, package_info, 
    cols_id = "Name",
    cols_remove = "Molecule Name",
    cols_pka = c("G+_pH7.4_S+Acidic_pKa", "G+_pH7.4_S+Mixed_pKa", "G+_pH7.4_S+Basic_pKa"),
    cols_nolog = c("G+_pH7.4_S+pH_Satd", "Exp_pKa-Sirius PKA1"),
    relevel_moka_status = TRUE
  )
  
  test_raw <- import_data(test_file)
  test_set <- process_test(test_raw, train_set$variables, cols_id = "Name", cols_remove = c("G+_pH7.4_SupSatn", "G+_pH7.4_BBB_Filter", "G+_pH7.4_AcidAtoms", "G+_pH7.4_BaseAtoms"))
  
  pred <- predict_new_rf_exps(train_set, EXP_VARIABLES, test_set$tab)
}


