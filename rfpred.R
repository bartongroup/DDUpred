suppressPackageStartupMessages(library(optparse))
source("R/main.R")

option_list <- list(
  make_option(c("-t", "--training-file"), action="store", default=NA, type='character',
              help="Training data file. A CSV file with training data."),
  make_option(c("-i", "--info-file"), action="store", default=NA, type='character',
              help="Training data variable info file. A CSV file with two columns: 'variable' and 'type'. The first column contains variable names, the second 'id' for identifier column, 'response' for response variables or 'none' for variables not to be used."),
  make_option(c("-s", "--test-file"), action="store", default=NA, type='character',
              help="Test data file"),
  make_option(c("-o", "--output-dir"), action="store", default=NA, type='character',
              help="Output directory where all results will be written."),
  make_option(c("-m", "--min-good"), action="store", default=NA, type='integer',
              help="Minimum number of good values in each variable.")
)

op <- OptionParser(option_list=option_list)
opt <- parse_args(op)

if(is.na(opt$`training-file`) | is.na(opt$`info-file`) | is.na(opt$`test-file`) | is.na(opt$`output-dir`) | is.na(opt$`min-good`)) {
  print_help(op)
  stop("Option(s) missing")
}

# Predictions

pred <- ddu_prediction(opt$`training-file`, opt$`info-file`, opt$`test-file`, verbose=TRUE, min_unique=2, min_good=opt$`min-good`, max_cat_levels=10)

# Write results

write_results(pred, opt$`output-dir`)


