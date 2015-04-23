
library(data.table)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  #files <- list.files(directory, pattern='*.csv', full.names=TRUE)
  files <- character()

  for (i in id) {
    files <- append(files, sprintf('%s/%03d.csv', directory, i))
  }

  f_content <- lapply(files, read.csv)
  data <- rbindlist(f_content)
  p_data <- data[[pollutant]]

  na <- is.na(p_data)
  mean(p_data[!na])
}