#!/usr/bin/env Rscript

# renv::load("/Users/mgierlinski/Projects/DDUpred")

suppressPackageStartupMessages({
  library(optparse)
  library(drupr)
  library(tidyverse)
})

option_list <- list(
  make_option(c("-i", "--input-file"), action = "store", default = NA, type = 'character',
              help = "Input CSV file."),
  make_option(c("-r", "--rename-file"), action = "store", default = "test_data/lombardo_rename.csv", type = 'character',
              help = "File with rename information."),
  make_option(c("-o", "--output-file"), action = "store", default = NA, type = 'character',
              help = "Output file name.")
)

op <- OptionParser(option_list = option_list)
opt <- parse_args(op)

if (is.na(opt$`input-file`) |  is.na(opt$`output-file`)) {
  print_help(op)
  stop("Option(s) missing")
}

rename_for_vd(opt$`input-file`, opt$`rename-file`, verbose = TRUE) %>% 
  write_csv(opt$`output-file`)



