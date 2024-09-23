#!/usr/bin/env Rscript

# renv::load("/Users/mgierlinski/Projects/DDUpred")

suppressPackageStartupMessages({
  library(optparse)
  library(drupr)
  library(tidyverse)
})

option_list <- list(
  make_option(c("-d", "--descriptor-file"), action = "store", default = NA, type = 'character',
              help = "Descriptor data Excel file, contains all data, including pH7.4 data."),
  make_option(c("-p", "--ph2-file"), action = "store", default = NA, type = 'character',
              help = "Excel file with pH2 data."),
  make_option(c("-m", "--moka-file"), action = "store", default = NULL, type = 'character',
              help = "Excel file with Moka designations data (optional)."),
  make_option(c("-o", "--output-file"), action = "store", default = NA, type = 'character',
              help = "Output file name (CVS file).")
)

op <- OptionParser(option_list = option_list)
opt <- parse_args(op)

if (is.na(opt$`descriptor-file`) | is.na(opt$`ph2-file`) | is.na(opt$`output-file`)) {
  print_help(op)
  stop("Option(s) missing")
}

merged <- merge_ph(opt$`descriptor-file`, opt$`ph2-file`, opt$`moka-file`, verbose = TRUE)

cat(paste("\nWriting output file", opt$`output-file`, "\n"))
write_csv(merged, opt$`output-file`)


