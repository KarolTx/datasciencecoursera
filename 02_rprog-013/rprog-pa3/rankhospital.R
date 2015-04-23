rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  if (!(state %in% data$State)) {
    stop('invalid state')
  }
  
  if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }

  if (!((num %in% c('best', 'worst')) | is.numeric(num))) {
    stop('invalid num')
  }
  outcome_split <- unlist(strsplit(outcome, ' '))
  
  if (length(outcome_split) == 1) {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2))
  } else {
    col <- sprintf('Hospital.30.Day.Death..Mortality..Rates.from.%s%s.%s%s', toupper(substring(outcome_split[1], 1, 1)), substring(outcome_split[1], 2), toupper(substring(outcome_split[2], 1, 1)), substring(outcome_split[2], 2))
  }

  # state restriction and column restriction
  data_sub <- data[data$State == state,c('Hospital.Name', col)]
  # converting to numbers
  suppressWarnings(data_sub[, c(col)] <- as.numeric(data_sub[, c(col)]))
  # removing NAs
  data_sub <- data_sub[complete.cases(data_sub),]
  # ordering
  data_order <- order(data_sub[[col]], data_sub$Hospital.Name)

  if (num == 'best') {
    return(head(data_sub[data_order,]$Hospital.Name, 1))
  }

  if (num == 'worst') {
    return(tail(data_sub[data_order,]$Hospital.Name, 1))
  }

  if (num > length(data_order)) {
    return(NA)
  } else {
    return((data_sub[data_order,]$Hospital.Name)[num])
  }
}