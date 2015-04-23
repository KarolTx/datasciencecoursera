complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  obs <- outer(id, 1:2, function(x, y) 0 * x * y)

  for (i in 1:length(id)) {
    content <- read.csv(sprintf('%s/%03d.csv', directory, id[i]))
    obs[i,] <- c(id[i], sum(complete.cases(content)))
  }

  obs <- data.frame(obs)
  names(obs) <- c('id', 'nobs')
  obs
}