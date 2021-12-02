import_data <- function(file, verbose=FALSE) {
  if(verbose) cat(paste("Reading data from", file, "\n"))
  d <- read_csv(file, col_types = cols(), na=c("", "NA", "n/a", "N/A", "None"), guess_max = 10000, progress = FALSE)
  if(verbose) cat(paste("   ", nrow(d), "rows,", ncol(d), "columns\n"))
  return(d)
}

