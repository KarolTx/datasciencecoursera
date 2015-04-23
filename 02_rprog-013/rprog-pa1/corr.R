corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source('complete.R')

  obs <- complete(directory)
  relevant_obs <- obs[obs$nobs > threshold,]
  ids <- relevant_obs$id

  files <- character()

  for (i in ids) {
    files <- append(files, sprintf('%s/%03d.csv', directory, i))
  }

  read_compute_cor <- function(file_name) {
    content <- read.csv(file_name)
    cor(content$nitrate, content$sulfate, use='complete.obs')
  }

  unlist(lapply(files, read_compute_cor))
}
